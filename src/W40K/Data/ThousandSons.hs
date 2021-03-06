module W40K.Data.ThousandSons where

import Prelude hiding (Functor(..), Monad(..), sequence)
import Control.Lens
import Data.List (isInfixOf)

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Core.Util (filteredOn)

import W40K.Data.Common
import qualified W40K.Data.Chaos as Chaos


otherworldlyPresence :: ModelEffect
otherworldlyPresence = as_model %~ stack [model_inv -~ 1, model_name <>~ " (OwP)"]

glamourOfTzeentch :: ModelEffect
glamourOfTzeentch = as_model %~ stack [model_mods.mod_tobehit -~ 1, model_name <>~ " (GoT)"]

weaverOfFates :: ModelEffect
weaverOfFates = Chaos.weaverOfFates

gazeOfMagnus :: PsychicPower
gazeOfMagnus = offensivePsychic 5 $ \_ _ cv ->
    if cv >= 10 then
      MortalWounds two_d6
    else
      MortalWounds d6
  where
    two_d6 = sumProbs [d6, d6]

infernalFusillade :: Effect
infernalFusillade =
    filteredOn (em_model.model_name) isRubricOrScarab
    .em_rw
    .mapped
    .filtered (\rw -> rw^.rw_class == RapidFire)
    .rw_shots %~ fmap (*2)
  where
    isRubricOrScarab name = isInfixOf "rubric" name || isInfixOf "scarab occult" name


-- PSYCHIC

ahrimanPsyker :: Psyker
ahrimanPsyker = defaultPsyker
  & psyker_cast_mod .~ Add 1
  & psyker_deny_mod .~ Add 1


magnusPsychicAura :: Psyker -> Psyker
magnusPsychicAura = psyker_cast_roll .~ sequence [d6rr1, d6rr1]


magnusPsyker :: Psyker
magnusPsyker = defaultPsyker
  & psyker_cast_mod .~ Add 2
  & psyker_deny_mod .~ Add 2
  & magnusPsychicAura


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
  & model_allIsDust   .~ True
  & model_ignoreHeavy .~ True
  & model_name        .~ "rubric marine"

scarabOccultModel :: Model
scarabOccultModel = rubricModel
  & model_att         +~ 1
  & model_ld          +~ 1
  & model_wnd         +~ 1
  & model_name        .~ "scarab occult"


magnusModel :: Model
magnusModel = daemonPrinceModel
  & model_str   .~ 8
  & model_tgh   .~ 7
  & model_att   .~ 7
  & model_wnd   .~ 18
  & model_name  .~ "magnus the red"


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
tzaangor = equipped tzaangorModel
  & em_ccw   .~ tzaangorBlades

twistbray :: EquippedModel
twistbray = tzaangor
  & em_model %~ stack [model_att +~ 1, model_ld +~ 1]
  & em_name  .~ "twistbray"

bolterRubric :: EquippedModel
bolterRubric = equipped rubricModel
  & em_rw    .~ [infernoBolter]

soulreaperRubric :: EquippedModel
soulreaperRubric = equipped rubricModel
  & em_rw    .~ [soulreaperCannon]

aspiringSorcerer :: EquippedModel
aspiringSorcerer = bolterRubric
  & em_model %~ stack [model_att +~ 1, model_ld +~ 1, model_allIsDust .~ False]
  & em_ccw   .~ forceStave
  & em_rw    .~ [infernoBoltPistol]
  & em_name  .~ "rubric aspiring sorcerer"

magnus :: EquippedModel
magnus = equipped magnusModel
  & em_ccw   .~ bladeOfMagnus
  & em_name  .~ "magnus the red"

buffedMagnus :: EquippedModel
buffedMagnus = magnus
  & em_model %~ glamourOfTzeentch
  & em_model %~ weaverOfFates
  & em_model %~ Chaos.prescience
  & em_model %~ Chaos.diabolicStrength
  & em_name  .~ "magnus the red (buffed)"


-- SQUADS

maleficTalonsDaemonPrince :: EquippedModel
maleficTalonsDaemonPrince = equipped daemonPrinceModel
  & em_ccw   .~ Chaos.twoMaleficTalons
  & em_name  .~ "daemon prince w/malefic talons"

tzaangors :: Int -> [EquippedModel]
tzaangors n = twistbray : replicate (n-1) tzaangor

rubricSquad :: Int -> [EquippedModel]
rubricSquad n = aspiringSorcerer : replicate (n-1) bolterRubric

soulreaperRubricSquad ::  Int -> [EquippedModel]
soulreaperRubricSquad n = soulreaperRubric : rubricSquad (n-1)

-- scarabOccultSquad :: Int -> [EquippedModel]
-- scarabOccultSquad n = scarabAspiringSorcerer : replicate (n-1) bolterScarab
