module Gundyr.Commands.Reamojis where

import Calamity
import Calamity.Commands
import Control.Lens hiding (Context)
import Control.Monad
import Data.Flags
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Database.Beam (runDelete, runInsert, runSelectReturningOne, runSelectReturningList)
import DiPolysemy
import Gundyr.Db
import qualified Polysemy as P
import TextShow (showtl)

requireAdmin :: BotC r => P.Sem (DSLState r) a -> P.Sem (DSLState r) a
requireAdmin = requires' "admin :)" $ \ctx -> do
  case (ctx ^. #member, ctx ^. #guild) of
    (Just mem, Just guild) -> case permissionsIn guild mem `containsAll` administrator of
                           True -> return Nothing
                           _ -> return $ Just "not an admin"
    _ -> return $ Just "not guild member or not in a guild"

reamojiGroup :: (BotC r, P.Member DBEff r) => P.Sem (DSLState r) ()
reamojiGroup = void
  . help (const "Group for reamojis")
  . requireAdmin
  . group "reamoji"
  $ do
    void $ help (const "send a new message to the channel with a label, and store it in the db") $
      command @'[ Named "channel" (Snowflake Channel), Named "label" L.Text
                , Named "message" (KleenePlusConcat L.Text)] "create-msg" $
      \ctx ch alias msgText -> do
        info @L.Text "hello there"
        label <- usingConn . runSelectReturningOne . getLabel $ alias
        case label of
          Just _ -> void . tell @L.Text ctx $ "Alias already exists."
          Nothing -> do
            eithermsg <- tell @L.Text ch msgText
            case eithermsg of
              (Right msg) -> do
                void . usingConn . runInsert $ addLabel (msg ^. #id) ch alias 
              _ -> return ()

    void $ help (const "add an emoji - role pair to a labeled message") $
      command @'[ Named "label" L.Text, Named "emoji" RawEmoji,
        Named "role" (Snowflake Role)] "add" $ \ctx alias emo role -> do
          label <- usingConn . runSelectReturningOne . getLabel $ alias
          case label of
            Nothing -> void . tell @L.Text ctx $
              "label " <> alias <> " does not exist :("
            Just (Label mid chid _) -> do
              reamoji' <- usingConn . runSelectReturningOne $ getReamoji mid emo
              case reamoji' of
                Nothing -> do
                  usingConn . runInsert $ addReamoji mid emo role (Roles []) (Roles [])
                  void . invoke $ CreateReaction chid mid emo
                  void . tell @L.Text ctx $ "done"
                (Just _) -> void $ tell @L.Text ctx $ showtl emo <> " is already in use for the label"

    void . help (const "remove an emoji - role pair from a labeled message") $
      command @'[ Named "label" L.Text, Named "emoji" RawEmoji] "remove" $
        \ctx alias emo -> do
          label <- usingConn . runSelectReturningOne . getLabel $ alias
          case label of
            Nothing -> void . tell @L.Text ctx $
              "label " <> alias <> " does not exist :("
            Just (Label mid chid _) -> do
              reamoji' <- usingConn . runSelectReturningOne $ getReamoji mid emo
              case reamoji' of
                Nothing -> void $ tell @L.Text ctx $ showtl emo <> " is not being used for the label"
                (Just _) -> do
                  usingConn . runDelete $ removeReamojiByIdEmoji mid emo
                  void . invoke $ DeleteOwnReaction chid mid emo
                  void . tell @L.Text ctx $ "removed " <> showtl emo <> " for label " <> alias

    void . help (const "delete a labeled message") $ command @'[ Named "label" L.Text ] "delete" $
      \ctx alias -> do
        label <- usingConn . runSelectReturningOne . getLabel $ alias
        case label of
          Nothing -> void . tell @L.Text ctx $
            "label " <> alias <> " does not exist :("
          Just (Label mid chid _) -> do
            usingConn . runDelete $ removeLabel alias
            usingConn . runDelete $ removeReamojiById mid
            res <- invoke (DeleteMessage chid mid)
            case res of
              (Left x) -> void . tell @L.Text ctx $ "could not delete message: " <> (L.pack $ show x)
              _ -> void . tell @L.Text ctx $ "message deleted (hopefully)\
                \ along with db entries :))))"

    void . help (const "list all labeled messages") $ command @'[] "all-labels" $
      \ctx -> do
        info @L.Text "hello there"
        labels <- usingConn . runSelectReturningList $ allLabels
        void . tell @L.Text ctx . L.unlines $ _labelName <$> labels
        void . tell @L.Text ctx $ "done!"

    void . help (const "list emoji - role pairs for a label") $ command @'[Named "label" L.Text] "reamojis" $
      \ctx alias -> do
        label <- usingConn . runSelectReturningOne . getLabel $ alias
        case label of
          Nothing -> void . tell @L.Text ctx $
            "label " <> alias <> " does not exist :("
          Just (Label mid _ _) -> do
            reamojis <- usingConn . runSelectReturningList $ getReamojisForMsgId mid
            void . tell @L.Text ctx . L.pack . show $ reamojis
            void . tell @L.Text ctx $ "done!"

    void . help (const "update a labeled message text") $ command 
      @'[Named "label" L.Text, Named "msg" T.Text] "update" $ \ctx alias msg -> do
        label <- usingConn . runSelectReturningOne . getLabel $ alias
        case label of
          Nothing -> void . tell @L.Text ctx $
            "label " <> alias <> " does not exist :("
          Just (Label mid chid _) -> do
            void . invoke $ EditMessage chid mid (Just msg) Nothing
            void . tell @L.Text ctx $ "done!"

    void . help (const "react to a labeled message") $ command
      @'[Named "label" L.Text, Named "emoji" RawEmoji] "react" $ \ctx alias emo -> do
        label <- usingConn . runSelectReturningOne . getLabel $ alias
        case label of
          Nothing -> void . tell @L.Text ctx $
            "label " <> alias <> " does not exist :("
          Just (Label mid chid _) -> do
            void . invoke $ CreateReaction chid mid emo
            void . tell @L.Text ctx $ "done!"

    help (const "remove Gundyr's reaction from a labeled message") $ command
      @'[Named "label" L.Text, Named "emoji" RawEmoji] "unreact" $ \ctx alias emo -> do
        label <- usingConn . runSelectReturningOne . getLabel $ alias
        case label of
          Nothing -> void . tell @L.Text ctx $
            "label " <> alias <> " does not exist :("
          Just (Label mid chid _) -> do
            void . invoke $ DeleteOwnReaction chid mid emo
            void . tell @L.Text ctx $ "done!"

