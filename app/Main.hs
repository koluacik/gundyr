module Main where

import Calamity
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Gateway.Types
import Calamity.Metrics.Noop
import qualified Calamity.Types.Model.User as CU
import qualified Calamity.Types.Model.Channel.Guild.Text as CGT
import qualified Calamity.Types.Model.Channel.Message as CM
import Control.Monad
import Data.Maybe
import Data.Text.Lazy (Text, append, toStrict, pack)
import qualified DiPolysemy as DiP
import Polysemy
import Gundyr (runGundyr)

main :: IO ()
main = runGundyr
