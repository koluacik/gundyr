module Gundyr.Commands.Util where

import Calamity
import Calamity.Commands
import Control.Lens hiding (Context)
import Data.Bitraversable

memAndGuildFromCtx :: Context -> Maybe (Member, Guild)
memAndGuildFromCtx ctx = bisequence (ctx ^. #member, ctx ^. #guild)
