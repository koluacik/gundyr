module Gundyr.Db.Eff
  ( DBEff (..)
  , runDBEffPooled
  , usingConn
  ) where

import Polysemy
import Data.Pool
import Database.Beam.Sqlite (runBeamSqlite, SqliteM)
import Database.SQLite.Simple (Connection)

data DBEff m a where
  UsingConn :: SqliteM a -> DBEff m a

makeSem ''DBEff

runDBEffPooled :: forall r a. Member (Embed IO) r => Pool Connection -> Sem (DBEff ': r) a -> Sem r a
runDBEffPooled pool = interpret \case UsingConn m -> embed $ withResource pool (flip runBeamSqlite m)
