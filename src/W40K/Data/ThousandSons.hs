module W40K.Data.ThousandSons where

import Prelude hiding (Functor(..), Monad(..), sequence)
import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Core.Psychic

import W40K.Data.Common
import qualified W40K.Data.Chaos as Chaos


otherworldlyPresence :: Model -> Model
otherworldlyPresence = (model_inv -~ 1) . (model_name <>~ " (otherworldly presence)")

glamourOfTzeentch :: Model -> Model
glamourOfTzeentch = (model_mods.mod_tobehit -~ 1) . (model_name <>~ " (-1 to hit)")

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
  & model_inv         .~ 5
  & model_ignoreHeavy .~ True
  & model_name        .~ "rubric marine"

magnusModel :: Model
magnusModel = daemonPrinceModel
  & model_str   .~ 8
  & model_tgh   .~ 7
  & model_att   .~ 7
  & model_wnd   .~ 18
  & model_name  .~ "magnus the red"

magnusPsychicAura :: Psyker -> Psyker
magnusPsychicAura = psyker_cast_roll .~ sequence [d6rr1, d6rr1]

magnusPsyker :: Psyker
magnusPsyker = defaultPsyker
  & psyker_cast_mod .~ Add 2
  & psyker_deny_mod .~ Add 2
  & magnusPsychicAura


-- RANGED WEAPONS

infernoBolter :: RngWeapon
infernoBolter = bolter
  & rw_ap   .~ -2
  & rw_name .~ "inferno boltgun"

infernoBoltPistol :: RngWeapon
infernoBoltPistol = boltPistol
  & rw_ap   .~ -2
  & rw_name .~ "inferno bolt pistol"

soulreaperCannon :: RngWeapon
soulreaperCannon = heavyBolter
  & rw_class .~ Heavy
  & rw_shots .~ return 4
  & rw_ap    .~ -3
  & rw_name  .~ "soulreaper cannon"


-- CC WEAPONS

tzaangorBlades :: CCWeapon
tzaangorBlades = basic_ccw
  & ccw_attBonus .~ Add 1
  & ccw_ap       .~ -1
  & ccw_name     .~ "tzaangor blades"

bladeOfMagnus :: CCWeapon
bladeOfMagnus = basic_ccw
  & ccw_strMod .~ Times 2
  & ccw_ap     .~ -4
  & ccw_dmg    .~ return 3
  & ccw_name   .~ "the blade of magnus"


-- EQUIPPED MODELS

tzaangor :: EquippedModel
tzaangor = basicEquippedModel tzaangorModel
  & em_ccw   .~ tzaangorBlades

twistbray :: EquippedModel
twistbray = tzaangor
  & em_model %~ (model_att +~ 1) . (model_ld +~ 1)
  & em_name  .~ "twistbray"

bolterRubric :: EquippedModel
bolterRubric = basicEquippedModel rubricModel
  & em_rw    .~ [infernoBolter]

soulreaperRubric :: EquippedModel
soulreaperRubric = basicEquippedModel rubricModel
  & em_rw    .~ [soulreaperCannon]

aspiringSorcerer :: EquippedModel
aspiringSorcerer = bolterRubric
  & em_model %~ (model_att +~ 1) . (model_ld +~ 1) . (model_allIsDust .~ False)
  & em_ccw   .~ forceStave
  & em_rw    .~ [infernoBoltPistol]
  & em_name  .~ "aspiring sorcerer"

magnus :: EquippedModel
magnus = basicEquippedModel magnusModel
  & em_ccw   .~ bladeOfMagnus
  & em_name  .~ "magnus the red"

buffedMagnus :: EquippedModel
buffedMagnus = magnus
  & em_model %~ glamourOfTzeentch
  & weaverOfFates
  & Chaos.prescience
  & Chaos.diabolicStrength
  & em_name  .~ "magnus the red (buffed)"


-- SQUADS

maleficTalonsDaemonPrince :: EquippedModel
maleficTalonsDaemonPrince = basicEquippedModel daemonPrinceModel
  & em_ccw   .~ Chaos.twoMaleficTalons
  & em_name  .~ "daemon prince w/malefic talons"

tzaangors :: Int -> [EquippedModel]
tzaangors n = twistbray : replicate (n-1) tzaangor

rubricSquad :: Int -> [EquippedModel]
rubricSquad n = aspiringSorcerer : replicate (n-1) bolterRubric

soulreaperRubricSquad ::  Int -> [EquippedModel]
soulreaperRubricSquad n = soulreaperRubric : rubricSquad (n-1)
