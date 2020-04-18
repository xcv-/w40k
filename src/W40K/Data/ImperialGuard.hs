module W40K.Data.ImperialGuard where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.Mechanics

import W40K.Data.Common


lemanRuss :: Model
lemanRuss = rhino
  & model_bs   .~ 4
  & model_str  .~ 7
  & model_tgh  .~ 8
  & model_wnd  .~ 12
  & model_ld   .~ 7
  & model_name .~ "leman russ"


baneblade :: Model
baneblade = lemanRuss
  & model_ws          .~ 5
  & model_str         .~ 9
  & model_wnd         .~ 26
  & model_att         .~ 9
  & model_ignoreHeavy .~ True
  & model_name        .~ "baneblade"
