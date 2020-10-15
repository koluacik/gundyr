{-# OPTIONS_GHC -Wno-orphans #-}

module Gundyr.Db.Schema
  ( ReamojiT (..)
  , Reamoji
  , LabelT (..)
  , Label
  , PrimaryKey (..)
  , BotDB
  , db
  ) where

import Calamity
import Control.Lens
import Database.Beam
import Database.Beam.Sqlite
import Gundyr.Db.Util

data LabelT f
  = Label
    { _labelId :: C f (Snowflake Message)
    , _labelChannelId :: C f (Snowflake Channel)
    , _labelName :: C f Alias
    } deriving (Generic, Beamable)

type Label = LabelT Identity
deriving instance Show Label

instance Table LabelT where
  data PrimaryKey LabelT f = LabelKey (C f (Snowflake Message))
    deriving (Generic, Beamable)
  primaryKey label = LabelKey (label ^. #_labelId)

type LabelKey' = PrimaryKey LabelT Identity
deriving instance Show LabelKey'


data ReamojiT f
  = Reamoji 
    { _reamojiId :: C f (Snowflake Message)
    , _reamojiEmoji :: C f RawEmoji
    , _reamojiIdFoo :: PrimaryKey LabelT f
    , _reamojiRole :: C f (Snowflake Role)
    , _reamojiPrereq :: C f Roles
    , _reamojiContradicts :: C f Roles
    } deriving (Generic, Beamable)

type Reamoji = ReamojiT Identity
deriving instance Show Reamoji

instance Table ReamojiT where
  data PrimaryKey ReamojiT f = ReamojiKey (C f (Snowflake Message)) (C f RawEmoji)
    deriving (Generic, Beamable)
  primaryKey msg = ReamojiKey (_reamojiId msg) (_reamojiEmoji msg)

data BotDB f = BotDB { reamojis :: f (TableEntity ReamojiT)
                     , labels :: f (TableEntity LabelT)
                     }
  deriving (Generic, Database Sqlite)

db :: DatabaseSettings Sqlite BotDB
db = defaultDbSettings
  `withDbModification` dbModification { reamojis = modifyTableFields tableModification
    { _reamojiIdFoo = LabelKey "id_foo" }}
