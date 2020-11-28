module Gundyr.React.Reamojis
  ( reactRawMessageReactionAddEvt
  , reactRawMessageReactionRemoveEvt
  ) where

import Calamity as C
--import Calamity.Cache.Eff (getBotUser)
import Control.Lens
import Control.Monad
import Data.Maybe
--import Data.Text.Lazy (Text)
import qualified Data.Vector.Unboxing as VU
import Database.Beam
import Gundyr.Db
import Gundyr.Util
--import DiPolysemy
import qualified Polysemy as P
import TextShow

reactRawMessageReactionAddEvt :: (BotC r, P.Member DBEff r) => P.Sem r (P.Sem r ())
reactRawMessageReactionAddEvt = react @'RawMessageReactionAddEvt
  \(Reaction uid _ mid gid' emo) -> unless (isNothing gid') do
    infot $ "user: " <> showtl uid <> " reacted to: " <> showtl mid
      <> " with: " <> showtl emo
    rctGuild <- fmap fromJust . upgrade @Guild . fromJust $ gid'
    rctMember <- fromJust <$> upgrade @Member
      (rctGuild ^. #id, coerceSnowflake' @Member uid)
    unless (rctMember ^. #bot == Just True) do
      reamoji' <- usingConn . runSelectReturningOne . select $ reamojiByIdAndEmoji mid emo
      case reamoji' of
        Nothing -> return ()
        Just reamoji -> do
          let memroles = rctMember ^. #roles
              roleid = reamoji ^. #role_id
          rolePrereqs <- fmap (^. #role_id2) <$> (usingConn .
            runSelectReturningList . select $ allPrereqs roleid)
          roleContradicts <- fmap (^. #role_id2) <$> (usingConn .
            runSelectReturningList . select $ allContradicts roleid)
          infot "checking conditions for role"
          when (all (`VU.elem` memroles) rolePrereqs &&
           VU.all (not . (`elem` roleContradicts)) memroles)
                     $ do
                       infot "giving role"
                       void . invoke $
                         AddGuildMemberRole rctMember rctMember roleid

reactRawMessageReactionRemoveEvt :: (BotC r, P.Member DBEff r) => P.Sem r (P.Sem r ())
reactRawMessageReactionRemoveEvt = react @'RawMessageReactionRemoveEvt
  \(Reaction uid _ mid gid' emo) -> do
    infot $
      "user: " <> showtl uid <> " removed reaction: " <> showtl emo <> " from: "
      <> showtl mid
    rctGuild <- fmap fromJust . upgrade @Guild . fromJust $ gid'
    rctMember <- fromJust <$> upgrade @Member
      (rctGuild ^. #id, coerceSnowflake' @Member uid)
    unless (rctMember ^. #bot == Just True) do
      reamoji' <- usingConn . runSelectReturningOne . select $ reamojiByIdAndEmoji mid emo
      case reamoji' of
        Nothing -> return ()
        Just reamoji -> do
          let roleid = reamoji ^. #role_id
          infot "removing role"
          void . invoke $ RemoveGuildMemberRole rctMember rctMember roleid
