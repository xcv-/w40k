module W40K.Data.ThousandSons where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Core.Psychic

import W40K.Data.Common
import qualified W40K.Data.Chaos as Chaos


allIsDust :: Modifier
allIsDust = em_model.model_mods.mod_tosave +~ 1

glamourOfTzeentch :: Modifier
glamourOfTzeentch = em_model.model_mods.mod_tobehit -~ 1

weaverOfFates :: Modifier
weaverOfFates = Chaos.weaverOfFates

gazeOfMagnus :: PsychicPower
gazeOfMagnus = smite
  & power_inflictMortalWounds .~ \_ _ cv ->
      if cv >= 10 then two_d6 else d6
  where
    two_d6 = sumProbs [d6, d6]



-- MODELS

exaltedSorcererModel :: Model
exaltedSorcererModel = meq
  & model_ws   .~ 2
  & model_bs   .~ 2
  & model_wnd  .~ 5
  & model_att  .~ 4
  & model_ld   .~ 9
  & model_save .~ 3
  & model_inv  .~ 5
  & model_mods.mod_rrtohit .~ RerollOnes
  & model_name .~ "exalted sorcerer"

daemonPrinceModel :: Model
daemonPrinceModel = Chaos.daemonPrinceModel
  & model_inv   .~ 4
  & model_name  .~ "TS daemon prince"

tzaangorModel :: Model
tzaangorModel = meq
  & model_bs   .~ 4
  & model_ld   .~ 7
  & model_save .~ nosave
  & model_inv  .~ 5
  & model_name .~ "tzaangor"

rubricModel :: Model
rubricModel = meq
  & model_inv  .~ 5
  & model_name .~ "rubric marine"

magnusModel :: Model
magnusModel = daemonPrinceModel
  & model_str   .~ 8
  & model_tgh   .~ 7
  & model_att   .~ 7
  & model_wnd   .~ 18
  & model_name  .~ "magnus the red"

magnusCasting :: PsykerCasting
magnusCasting = asPsyker magnusModel
  & cast_bonus .~ Add 2


-- RANGED WEAPONS

infernoBolter :: RngWeapon
infernoBolter = bolter
  & rw_ap   .~ -2
  & rw_name .~ "inferno boltgun"

infernoBoltPistol :: RngWeapon
infernoBoltPistol = boltPistol
  & rw_ap   .~ -2
  & rw_name .~ "inferno bolt pistol"


-- CC WEAPONS

bladeOfMagnus :: CCWeapon
bladeOfMagnus = basic_ccw
  & ccw_strMod .~ Times 2
  & ccw_ap     .~ -4
  & ccw_dmg    .~ return 3
  & ccw_name   .~ "the blade of magnus"


-- EQUIPPED MODELS

bolterRubric :: EquippedModel
bolterRubric = basicEquippedModel rubricModel
  & em_rw .~ [infernoBolter]

aspiringSorcerer :: EquippedModel
aspiringSorcerer = basicEquippedModel rubricModel
  & em_model %~ (model_att +~ 1) . (model_ld +~ 1)
  & em_ccw   .~ forceSword
  & em_rw    .~ [infernoBoltPistol]

magnus :: EquippedModel
magnus = basicEquippedModel magnusModel
  & em_ccw   .~ bladeOfMagnus
  & em_name  .~ "magnus the red"

buffedMagnus :: EquippedModel
buffedMagnus = magnus
  & glamourOfTzeentch
  & weaverOfFates
  & Chaos.prescience
  & Chaos.diabolicStrength


-- SQUADS

rubricSquad :: Int -> [EquippedModel]
rubricSquad n = aspiringSorcerer : replicate (n-1) bolterRubric
