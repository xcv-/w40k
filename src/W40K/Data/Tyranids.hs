module W40K.Data.Tyranids where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens
import Data.List (isInfixOf)

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common


kronosAdaptation :: Modifier
kronosAdaptation = em_model.model_rng_mods.mod_rrtohit <>~ RerollOnes

pathogenicSlime :: Modifier
pathogenicSlime = em_rw.mapped.rw_dmg %~ fmap (+1)

symbioticTargeting :: Modifier
symbioticTargeting = em_model.model_rng_mods.mod_tohit +~ 1

singleMindedAnnihilation :: Modifier
singleMindedAnnihilation = em_rw %~ \rws -> [rw | rw <- rws, _ <- [(), ()]]

weaponBeast :: Modifier
weaponBeast = em_rw %~ \rws -> [rw | rw <- rws, _ <- [(), ()]]

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
  & model_class .~ Monster
  & model_ws    .~ 2
  & model_bs    .~ 3
  & model_str   .~ 6
  & model_tgh   .~ 7
  & model_wnd   .~ 10
  & model_att   .~ 4
  & model_ld    .~ 10
  & model_inv   .~ 4
  & model_name  .~ "hive tyrant"

swarmlord :: Model
swarmlord = hiveTyrant
  & model_str     .~ 8
  & model_wnd     .~ 12
  & model_att     .~ 6
  & model_rng_inv .~ 4
  & model_cc_inv  .~ 3
  & model_name    .~ "swarmlord"

hiveGuardModel :: Model
hiveGuardModel = meq
  & model_str    .~ 4
  & model_tgh    .~ 5
  & model_wnd    .~ 3
  & model_att    .~ 2
  & model_ld     .~ 7
  & model_name   .~ "hive guard"

exocrineModel :: Model
exocrineModel = meq
  & model_class .~ Monster
  & model_att   .~ 3
  & model_ws    .~ 4
  & model_bs    .~ 4
  & model_str   .~ 7
  & model_tgh   .~ 8
  & model_wnd   .~ 12
  & model_ld    .~ 6
  & model_name  .~ "exocrine"

tyrannofexModel :: Model
tyrannofexModel = exocrineModel
  & model_att   .~ 4
  & model_wnd   .~ 14
  & model_ld    .~ 7
  & model_name  .~ "tyrannofex"

-- RANGED WEAPONS

impalerCannon :: RngWeapon
impalerCannon = krakMissile
  & rw_shots .~ return 2
  & rw_dmg   .~ d3
  & rw_name  .~ "impaler cannon"

bioPlasmicCannon :: RngWeapon
bioPlasmicCannon = lascannon
  & rw_shots .~ return 6
  & rw_str   .~ 7
  & rw_dmg   .~ return 2
  & rw_name  .~ "bioplasmic cannon"

ruptureCannon :: RngWeapon
ruptureCannon = lascannon
  & rw_shots .~ return 3
  & rw_str   .~ 10
  & rw_name  .~ "rupture cannon"

-- CC WEAPONS

powerfulLimbs :: CCWeapon
powerfulLimbs = basic_ccw
  & ccw_ap    .~ -2
  & ccw_dmg   .~ return 2
  & ccw_name  .~ "powerful limbs"


-- EQUIPPED MODELS

hiveGuard :: RngWeapon -> EquippedModel
hiveGuard rw = basicEquippedModel hiveGuardModel
  & em_rw .~ [rw]

exocrine :: EquippedModel
exocrine = basicEquippedModel exocrineModel
  & em_rw  .~ [bioPlasmicCannon]
  & em_ccw .~ powerfulLimbs

tyrannofex :: RngWeapon -> EquippedModel
tyrannofex rw = basicEquippedModel tyrannofexModel
  & em_rw  .~ [rw]
  & em_ccw .~ powerfulLimbs


-- SQUADS

hiveGuardSquad :: Int -> RngWeapon -> [EquippedModel]
hiveGuardSquad n rw = replicate n (hiveGuard rw)
