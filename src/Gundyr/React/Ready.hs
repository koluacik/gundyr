module Gundyr.React.Ready
  ( readyRct
  ) where

import Calamity as C
import Calamity.Gateway.Types
import qualified Polysemy as P

readyRct :: BotC r => P.Sem r (P.Sem r ())
readyRct = react @'ReadyEvt \_ -> do
  sendPresence
            StatusUpdateData 
              { since = Nothing
              , game = Just $ activity "an interesting game" Game
              , status = "online"
              , afk = False
              }
