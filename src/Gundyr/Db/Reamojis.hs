module Gundyr.Db.Reamojis
  ( addLabel
  , allLabels
  , getLabel
  , addReamoji
  , getMsgIdForAlias
  , getReamoji
  , getReamojisForAlias
  , getReamojisForMsgId
  , removeLabel
  , removeReamojiById
  , removeReamojiByIdEmoji
  )where

import Calamity
import Control.Lens
--import Data.Text.Lazy (Text)
import Database.Beam
import Database.Beam.Sqlite as Sq
import Gundyr.Db.Schema
import Gundyr.Db.Util

addLabel :: Snowflake Message
         -> Snowflake Channel
         -> Alias
         -> SqlInsert Sq.Sqlite LabelT
addLabel mid chid alias =
  insert (db ^. #labels) (insertValues [Label mid chid alias])

allLabels :: SqlSelect Sq.Sqlite Label
allLabels = select (all_ $ db ^. #labels)

getLabel :: Alias -> SqlSelect Sq.Sqlite Label
getLabel alias = select $ 
  filter_ (\r -> (r ^. #_labelName) ==. val_ alias)
  (all_ $ db ^. #labels)

addReamoji :: Snowflake Message
           -> RawEmoji
           -> Snowflake Role
           -> Roles
           -> Roles
           -> SqlInsert Sq.Sqlite ReamojiT
addReamoji mid emoji role prereq contradicts =
  insert (db ^. #reamojis) (insertValues [Reamoji mid emoji (LabelKey mid){-alias-} role prereq contradicts])

  {-
matchingAlias :: Alias -> Q Sq.Sqlite BotDB s (LabelT (QExpr Sq.Sqlite s))
matchingAlias alias = do
  mid <- all_ (db ^. #labels)
  guard_ (mid ^. #_labelName ==. val_ alias)
  return mid
  -}

getMsgIdForAlias :: Alias -> SqlSelect Sq.Sqlite (Snowflake Message)
getMsgIdForAlias alias = select $ do
  mid <- all_ (db ^. #labels)
  guard_ (mid ^. #_labelName ==. val_ alias)
  pure (mid ^. #_labelId)

getReamojisForAlias :: Alias -> SqlSelect Sq.Sqlite Reamoji
getReamojisForAlias alias = select $ do
  labels' <- all_ (db ^. #labels)
  reamojis' <- all_ (db ^. #reamojis)
  guard_ (labels' ^. #_labelName ==. val_ alias)
  guard_ ((reamojis' ^. #_reamojiIdFoo) `references_` labels')
  return reamojis'

getReamojisForMsgId :: Snowflake Message -> SqlSelect Sq.Sqlite Reamoji
getReamojisForMsgId mid = select $
    filter_
    (\r -> (r ^. #_reamojiId) ==. (val_ mid))
    (all_ $ db ^. #reamojis)

getReamoji :: Snowflake Message -> RawEmoji -> SqlSelect Sq.Sqlite Reamoji
getReamoji mid emo =
  select $
    filter_
    (\r -> (r ^. #_reamojiId) ==. (val_ mid) &&. (r ^. #_reamojiEmoji) ==. (val_ emo))
    (all_ $ db ^. #reamojis)

removeReamojiByIdEmoji :: Snowflake Message -> RawEmoji -> SqlDelete Sq.Sqlite ReamojiT
removeReamojiByIdEmoji mid emo =
  delete (db ^. #reamojis) 
  (\r -> (r ^. #_reamojiId) ==. val_ mid &&. (r ^. #_reamojiEmoji) ==. val_ emo)

removeLabel :: Alias -> SqlDelete Sq.Sqlite LabelT
removeLabel alias = 
  delete (db ^. #labels) 
  (\r -> (r ^. #_labelName) ==. val_ alias)

removeReamojiById :: Snowflake Message -> SqlDelete Sq.Sqlite ReamojiT
removeReamojiById mid =
  delete (db ^. #reamojis) 
  (\r -> (r ^. #_reamojiId) ==. val_ mid)

  {-

updateReamojiRole :: Snowflake Message -> RawEmoji -> Snowflake Role -> SqlUpdate Sq.Sqlite ReamojiT
updateReamojiRole mid emo role =
  update (db ^. #reamojis)
  (\r -> r ^. #_reamojiRole <-. val_ role)
  (\r -> (r ^. #_reamojiId) ==. val_ mid &&. (r ^. #_reamojiEmoji) ==. val_ emo)

updateReamojiRole' :: Alias -> RawEmoji -> Snowflake Role -> SqlUpdate Sq.Sqlite ReamojiT
updateReamojiRole' alias emo role = 
  update (db ^. #reamojis)
  (\r -> r ^. #_reamojiRole <-. val_ role)
  (\r -> (r ^. #_reamojiAlias) ==. val_ alias &&. (r ^. #_reamojiEmoji) ==. val_ emo)

updateReamojiPrereq :: Snowflake Message -> RawEmoji -> Roles -> SqlUpdate Sq.Sqlite ReamojiT
updateReamojiPrereq mid emo prereqs =
  update (db ^. #reamojis)
  (\r -> r ^. #_reamojiPrereq <-. val_ prereqs)
  (\r -> (r ^. #_reamojiId) ==. val_ mid &&. (r ^. #_reamojiEmoji) ==. val_ emo)

updateReamojiPrereq' :: Alias -> RawEmoji -> Roles -> SqlUpdate Sq.Sqlite ReamojiT
updateReamojiPrereq' alias emo prereqs =
  update (db ^. #reamojis)
  (\r -> r ^. #_reamojiPrereq <-. val_ prereqs)
  (\r -> (r ^. #_reamojiAlias) ==. val_ alias &&. (r ^. #_reamojiEmoji) ==. val_ emo)

updateReamojiContradicts :: Snowflake Message -> RawEmoji -> Roles -> SqlUpdate Sq.Sqlite ReamojiT
updateReamojiContradicts mid emo contradicts =
  update (db ^. #reamojis)
  (\r -> r ^. #_reamojiContradicts <-. val_ contradicts)
  (\r -> (r ^. #_reamojiId) ==. val_ mid &&. (r ^. #_reamojiEmoji) ==. val_ emo)

updateReamojiContradicts' :: Alias -> RawEmoji -> Roles -> SqlUpdate Sq.Sqlite ReamojiT
updateReamojiContradicts' alias emo contradicts =
  update (db ^. #reamojis)
  (\r -> r ^. #_reamojiContradicts <-. val_ contradicts)
  (\r -> (r ^. #_reamojiAlias) ==. val_ alias &&. (r ^. #_reamojiEmoji) ==. val_ emo)
  -}
