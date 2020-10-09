module Gundyr where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Metrics.Noop
import qualified Data.Text.Lazy.IO as LIO
import qualified DiPolysemy as DiP
import Gundyr.Command
import Polysemy

runGundyr :: IO ()
runGundyr = do
  myToken <- LIO.readFile "tokenFile"
  res <- runFinal
    . embedToFinal
    . DiP.runDiNoop
    . runCacheInMemoryNoMsg
    . runMetricsNoop
    . useConstantPrefix "!"
    . runBotIO (BotToken myToken)
    $ do
      addCommands $ do
        helpCommand
        getUER
        giveRole
  case res of
    (Just (StartupError x)) -> putStrLn x
    _ -> putStrLn "ok!"
