{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Gundyr (runGundyr) where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Metrics.Noop
import Control.Monad
import Data.Flags
import Data.Pool (createPool)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO
import Database.SQLite.Simple --(open, close, Connection)
import qualified Di
import qualified DiPolysemy as DiP
import Gundyr.Commands
import Gundyr.Db
import Gundyr.React
import Polysemy
import System.IO

runGundyr :: IO ()
runGundyr = Di.new \di -> do
  pool <- createPool (open' "test.db") close 3 0.5 30
  myToken <- withFile "tokenFile" ReadMode LIO.hGetLine
  res <- runFinal
    . embedToFinal
    . DiP.runDiToIO di
    . runDBEffPooled pool
    . runCacheInMemoryNoMsg
    . runMetricsNoop
    . useConstantPrefix "!"
    . runBotIO' (BotToken myToken) Nothing Nothing (Just allFlags)
    $ do
      addCommands $ do
        helpCommand
        reamojiGroup
        messageGroup
        roleGroup
        void $ command @'[] "good-bot" $ \ctx ->void $ tell @L.Text ctx "( u w u *)"
        void $ command @'[] "bad-bot" $ \ctx -> void $ tell @L.Text ctx "(OwO)"
        reactRawMessageReactionAddEvt
        reactRawMessageReactionRemoveEvt
        reactGuildMemberUpdateEvt
  case res of
    Just (StartupError x) -> putStrLn x
    _ -> putStrLn "ok!"

open' :: String -> IO Connection
open' path = do
  conn <- open path
  execute_ conn "PRAGMA foreign_keys = ON;"
  return conn
