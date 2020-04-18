module W40K.Data.DeathGuard where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Mechanics

import W40K.Data.Common
import qualified W40K.Data.Chaos as Chaos


-- MODIFIERS

disgusting :: Model -> Model
disgusting = model_fnp .~ 5

miasmaOfPestilence :: Model -> Model
miasmaOfPestilence = (model_mods.mod_tobehit -~ 1) . (model_name <>~ " (miasma)")

bladesOfPutrefaction :: EquippedModel -> EquippedModel
bladesOfPutrefaction =
    (em_model.model_cc_mods.mod_towound +~ 1)
    . (em_ccw.as_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 7) (WoundHookMortalWounds (return 1)))

putrescentVitality :: Model -> Model
putrescentVitality = (model_str +~ 1) . (model_tgh +~ 1) . (model_name <>~ " (putrescent vit.)")


-- MODELS

plagueMarineModel :: Model
plagueMarineModel = disgusting meq
  & model_tgh  .~ 5
  & model_name .~ "plague marine"

blightlordTerminatorModel :: Model
blightlordTerminatorModel = disgusting teq
  & model_tgh  .~ 5
  & model_inv  .~ 4
  & model_name .~ "blightlord terminator"

mortarionModel :: Model
mortarionModel = disgusting Chaos.daemonPrinceModel
  & model_str  .~ 8
  & model_tgh  .~ 7
  & model_wnd  .~ 18
  & model_ld   .~ 10
  & model_inv  .~ 4
  & model_name .~ "mortarion"


-- RANGED WEAPONS

lantern :: RngWeapon
lantern = plasmaPistol True
  & rw_dmg  .~ return 3
  & rw_name .~ "the lantern"


-- CC WEAPONS

silenceReaping :: CCWeapon
silenceReaping = basic_ccw
  & ccw_attBonus .~ Times 2
  & ccw_ap       .~ -2
  & ccw_name     .~ "silence (reaping scythe)"

silenceEviscerating :: CCWeapon
silenceEviscerating = basic_ccw
  & ccw_strMod   .~ Times 2
  & ccw_ap       .~ -3
  & ccw_dmg      .~ d6
  & ccw_name     .~ "silence (reaping scythe)"


-- EQUIPPED MODEL

mortarion :: CCWeapon -> EquippedModel
mortarion silence = basicEquippedModel mortarionModel
  & em_ccw .~ silence
  & em_rw  .~ [lantern]

buffedMortarion :: CCWeapon -> EquippedModel
buffedMortarion ccw = mortarion ccw
  -- & em_model %~ putrescentVitality
  & em_model %~ miasmaOfPestilence
  -- & bladesOfPutrefaction
  & em_name  .~ "DG-buffed mortarion"
