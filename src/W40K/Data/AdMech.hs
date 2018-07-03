module W40K.Data.AdMech where

import Prelude hiding (Functor(..), Monad(..))
import Data.List (isInfixOf)

import Control.Lens
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common

-- AURAS

dominusAura :: Aura
dominusAura = noAura & aura_rng.mod_rrtohit .~ RerollOnes

dataTether :: Modifier
dataTether = em_model.model_name <>~ " (data-tether)"

protectorDoctrinaImperative :: Modifier
protectorDoctrinaImperative em =
    if "data-tether" `isInfixOf` (em^.em_model.model_name) then
      em & em_model.model_rng_mods.mod_tohit +~ 2
    else
      em & em_model.model_rng_mods.mod_tohit +~ 1

conquerorDoctrinaImperative :: Modifier
conquerorDoctrinaImperative em =
    if "data-tether" `isInfixOf` (em^.em_model.model_name) then
      em & em_model.model_cc_mods.mod_tohit +~ 2
    else
      em & em_model.model_cc_mods.mod_tohit +~ 1

eliminationVolley :: Modifier
eliminationVolley em =
    if "kataphron" `isInfixOf` name || "kastelan" `isInfixOf` name then
      em & em_model.model_rng_mods.mod_tohit +~ 1
    else
      em
  where
    name = em^.em_model.model_name

wrathOfMars :: Modifier
wrathOfMars =
    em_rw.mapped.rw_weapon.w_hooks.hook_wound .~ Just (RollHook 6 (WoundHookMortalWounds (return 1)))


data Protocol = Aegis | Protector | Conqueror
  deriving (Eq, Ord, Enum, Bounded, Show)

withProtocol :: Protocol -> Modifier
withProtocol proto em =
  if "kastelan" `isInfixOf` (em^.em_model.model_name) then
    case proto of
      Aegis     -> em & em_model.model_mods.mod_tosave +~ 1
      Conqueror -> em & em_model.model_att  *~ 2
      Protector -> em & em_rw.mapped.rw_shots %~ fmap (*2)
  else
    em


-- MODELS

rangerModel :: Model
rangerModel = geq
  & model_bs   .~ 3
  & model_ld   .~ 6
  & model_save .~ 4
  & model_inv  .~ 6
  & model_name .~ "skitarii ranger"

vanguardModel :: Model
vanguardModel = rangerModel
  & model_name .~ "skitarii vanguard"

ruststalkerModel :: Model
ruststalkerModel = rangerModel
  & model_ws   .~ 3
  & model_str  .~ 4
  & model_wnd  .~ 2
  & model_att  .~ 2
  & model_name .~ "sicarian ruststalker"

ruststalkerPrincepsModel :: Model
ruststalkerPrincepsModel = ruststalkerModel
  & model_att  +~ 1
  & model_ld   +~ 1
  & model_name .~ "ruststalker princeps"

infiltratorModel :: Model
infiltratorModel = ruststalkerModel
  & model_ws  .~ 3
  & model_str .~ 4
  & model_wnd .~ 2
  & model_att .~ 2
  & model_name .~ "sicarian infiltrator"

infiltratorPrincepsModel :: Model
infiltratorPrincepsModel = infiltratorModel
  & model_att  +~ 1
  & model_ld   +~ 1
  & model_name .~ "infiltrator princeps"

kastelanModel :: Model
kastelanModel = rhino
  & model_ws      .~ 4
  & model_bs      .~ 4
  & model_att     .~ 3
  & model_wnd     .~ 6
  & model_ld      .~ 10
  & model_save    .~ 3
  & model_rng_inv .~ 5
  & model_name    .~ "kastelan robot"

dragoonModel :: Model
dragoonModel = rhino
  & model_ws      .~ 3
  & model_bs      .~ 3
  & model_str     .~ 5
  & model_tgh     .~ 6
  & model_att     .~ 3
  & model_wnd     .~ 6
  & model_ld      .~ 8
  & model_save    .~ 4
  & model_inv     .~ 6

