module W40K.Data.Tyranids where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common


-- MODELS

termagant :: Model
termagant = meq
  & model_ws   .~ 4
  & model_bs   .~ 4
  & model_str  .~ 3
  & model_tgh  .~ 3
  & model_ld   .~ 5
  & model_save .~ 6

tyranidWarrior :: Model
tyranidWarrior = meq
  & model_bs   .~ 4
  & model_wnd  .~ 3
  & model_att  .~ 3
  & model_ld   .~ 9
  & model_save .~ 4
  & model_name .~ "tyranid warrior"

hiveTyrant :: Model
hiveTyrant = meq
  & model_ws    .~ 2
  & model_bs    .~ 3
  & model_str   .~ 6
  & model_tgh   .~ 6
  & model_wnd   .~ 10
  & model_att   .~ 5
  & model_ld    .~ 10
  & model_inv   .~ 5
  & model_name  .~ "hive tyrant"

swarmlord :: Model
swarmlord = hiveTyrant
  & model_str     .~ 8
  & model_wnd     .~ 12
  & model_att     .~ 7
  & model_rng_inv .~ 5
  & model_cc_inv  .~ 5
  & model_name    .~ "swarmlord"

hiveGuard :: Model
hiveGuard = meq
  & model_str    .~ 4
  & model_tgh    .~ 5
  & model_wnd    .~ 3
  & model_att    .~ 2
  & model_ld     .~ 7
  & model_name   .~ "hive guard"

nonMovingExocrineModel :: Model
nonMovingExocrineModel = meq
  & model_att  .~ 3
  & model_ws   .~ 4
  & model_bs   .~ 3
  & model_str  .~ 7
  & model_tgh  .~ 8
  & model_wnd  .~ 12
  & model_ld   .~ 6
  & model_name .~ "non-moving exocrine"

-- RANGED WEAPONS

bioPlasmicCannon :: RngWeapon
bioPlasmicCannon = lascannon
  & rw_shots .~ return 6
  & rw_str   .~ 7
  & rw_ap    .~ -3
  & rw_dmg   .~ return 2
  & rw_name  .~ "bioplasmic cannon"


-- CC WEAPONS

powerfulLimbs :: CCWeapon
powerfulLimbs = basic_ccw
  & ccw_ap    .~ -2
  & ccw_dmg   .~ return 2
  & ccw_name  .~ "powerful limbs"


-- EQUIPPED MODELS

nonMovingExocrine :: EquippedModel
nonMovingExocrine = basicEquippedModel nonMovingExocrineModel
  & em_rw  .~ bioPlasmicCannon
  & em_ccw .~ powerfulLimbs
