module Gundyr.Commands.Permissions
  ( hasPermission
  , requireAdmin
  , requireManageRole
  ) where

import Calamity
import Calamity.Commands
import Data.Flags
import Data.Text.Lazy (Text)
import Gundyr.Commands.Util
import Polysemy (Sem)

hasPermission :: BotC r
              => Permissions
              -> Text
              -> Sem (DSLState r) a
              -> Sem (DSLState r) a
hasPermission perm noperm = requires' "manages roles" \ctx -> do
  case memAndGuildFromCtx ctx of
    Just (mem, guild) ->
      if permissionsIn guild mem `containsAll` perm
         then return Nothing
         else return (Just noperm)
    _ -> return $ Just "not guild member or not in a guild"

requireAdmin :: BotC r => Sem (DSLState r) a -> Sem (DSLState r) a
requireAdmin = hasPermission administrator "not an admin"

requireManageRole :: BotC r => Sem (DSLState r) a -> Sem (DSLState r) a
requireManageRole = hasPermission manageRoles "can't manage roles"
