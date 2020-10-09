module Gundyr.Command where

import Calamity
import Calamity.Commands
import Calamity.Commands.Command
import qualified Calamity.Commands.Context as CCC
import qualified Calamity.Types.Model.Channel.Message as CM
import Control.Monad
import Data.Maybe
import Gundyr.Util
import Polysemy

getUER :: BotC r => Sem (DSLState r) Command
getUER = command @'[Named "emoji" RawEmoji, Named "role" (Snowflake Role)] 
  "getUER" $ \ctx emo role -> 
    let emo' = case emo of UnicodeEmoji t -> t
                           CustomEmoji _ -> "abu"
     in void . tellt ctx $ "msg: " <>
       (showt . fromSnowflake . CM.id . CCC.message $ ctx) <> " emoji: " <> emo'
       <> " role: " <> showt role

giveRole :: BotC r => Sem (DSLState r) Command
giveRole = command @'[Named "user" (Snowflake User),
  Named "role" (Snowflake Role)] "giveRole" $ \ctx uid rid ->
    void . invoke $ AddGuildMemberRole (fromJust . CCC.guild $ ctx) uid rid
