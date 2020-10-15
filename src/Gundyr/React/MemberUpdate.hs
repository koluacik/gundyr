module Gundyr.React.MemberUpdate
  ( memRct
  ) where

import Calamity as C
import Control.Lens
import Control.Monad
import qualified Data.Vector.Unboxing as VU
import qualified Polysemy as P

memRct :: BotC r => P.Sem r (P.Sem r ())
memRct = react @'GuildMemberUpdateEvt \(mem, mem') ->
  if roleInit `VU.elem` (mem ^. #roles) && roleMember `VU.elem` (mem' ^. #roles)
     then void . invoke $ RemoveGuildMemberRole mem mem roleInit
     else return ()
       where
         roleInit = Snowflake @Role 764143270573375489
         roleMember = Snowflake @Role 763167512539955241
