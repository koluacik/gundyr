module Gundyr.Command
  ( getUER
  , module Gundyr.Commands.Reamojis
  ) where

import Calamity
import Calamity.Commands
import Calamity.Commands.Command
import qualified Calamity.Commands.Context as CCC
import qualified Calamity.Types.Model.Channel.Message as CM
import Control.Monad
import Gundyr.Util
import Gundyr.Commands.Reamojis
import Polysemy

getUER :: BotC r => Sem (DSLState r) Command
getUER = command @'[Named "emoji" RawEmoji, Named "role" (Snowflake Role)] 
  "getUER" $ \ctx emo role -> 
    let emo' = case emo of UnicodeEmoji t -> t
                           CustomEmoji _ -> "test"
     in void . tellt ctx $ "msg: " <>
       (showt . fromSnowflake . CM.id . CCC.message $ ctx) <> " emoji: " <> emo'
       <> " role: " <> showt role
