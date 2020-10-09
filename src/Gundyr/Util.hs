module Gundyr.Util
  ( tellt
  , showt
  ) where

import Calamity
import qualified Data.Text.Lazy as L (toStrict, pack)
import Data.Text.Lazy (Text)
import Polysemy

tellt :: (BotC r, Tellable t) => t -> Text -> Sem r (Either RestError Message)
tellt t m = tell t $ L.toStrict m 

showt :: Show a => a -> Text
showt = L.pack . show
