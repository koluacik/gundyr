module Gundyr.React where

import Calamity
--import Calamity.Cache.Eff
import Calamity.Gateway.Types
--import qualified Calamity.Types.Model.Channel.Message as CM
--import Control.Monad
--import Data.Maybe
--import qualified Data.Vector.Unboxing as VU
--import Gundyr.Util
import Polysemy

readyRct :: BotC r => Sem r (Sem r ())
readyRct = react @'ReadyEvt \_ -> do
  sendPresence
            StatusUpdateData 
              { since = Nothing
              , game = Just $ activity "DARK SOUL III" Game
              , status = "online"
              , afk = False
              }

{-
msgCreateRct :: BotC r => Sem r (Sem r ())
msgCreateRct = react @'MessageCreateEvt \msg -> do
  botId <- fromJust . (getID @User <$>) <$> getBotUser
  if not $ botId `VU.elem` (CM.mentions msg)
     then return ()
     else void . tellt msg $ "mid: " `L.append` (showt . getID @User $ msg)
-}
