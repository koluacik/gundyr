module Gundyr.React.Reamojis
  ( rawMsgReactionAddRct
  , rawMsgReactionRemoveRct
  ) where

import Calamity as C
import Calamity.Cache.Eff (getBotUser)
import Control.Monad
import Data.Maybe
import Database.Beam
import Gundyr.Db
import qualified Polysemy as P

rawMsgReactionAddRct :: (BotC r, P.Member DBEff r) => P.Sem r (P.Sem r ())
rawMsgReactionAddRct = react @'RawMessageReactionAddEvt 
  \(Reaction uid _ mid maybegid emo) ->
    when (maybegid /= Nothing) $ do
    row <- usingConn . runSelectReturningOne $ getReamoji mid emo
    case row of
      Nothing -> return ()
      Just (Reamoji _ _ _ role _ _) -> do
        gbu <- getBotUser
        when ((getID @User <$> gbu) /= (Just uid)) $
          void . invoke $ AddGuildMemberRole (fromJust maybegid) uid role
    return ()
    
rawMsgReactionRemoveRct :: (BotC r, P.Member DBEff r) => P.Sem r (P.Sem r ())
rawMsgReactionRemoveRct = react @'RawMessageReactionRemoveEvt
  \(Reaction uid _ mid maybegid emo) ->
    when (maybegid /= Nothing) $ do
    row <- usingConn . runSelectReturningOne $ getReamoji mid emo
    case row of
      Nothing -> return ()
      Just (Reamoji _ _ _ role _ _) -> do
        gbu <- getBotUser
        when ((getID @User <$> gbu) == (Just uid)) $
          void . invoke $ RemoveGuildMemberRole (fromJust maybegid) uid role
    return ()
