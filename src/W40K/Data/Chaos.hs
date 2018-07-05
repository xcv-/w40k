module W40K.Data.Chaos where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Core.Psychic

import W40K.Data.Common


-- MODIFIERS

diabolicStrength :: Modifier
diabolicStrength = em_model %~ (model_str +~ 2) . (model_att +~ 1)

prescience :: Modifier
prescience = em_model.model_mods.mod_tohit +~ 1

weaverOfFates :: Modifier
weaverOfFates = em_model.model_inv %~ \inv -> max (inv-1) 3


-- MODELS

daemonPrinceModel :: Model
daemonPrinceModel = meq
  & model_class .~ Monster
  & model_ws    .~ 2
  & model_bs    .~ 2
  & model_str   .~ 7
  & model_tgh   .~ 6
  & model_wnd   .~ 8
  & model_att   .~ 4
  & model_ld    .~ 10
  & model_save  .~ 3
  & model_inv   .~ 5
  & model_mods.mod_rrtohit .~ RerollOnes
  & model_name  .~ "daemon prince"

chaosMarineModel :: Model
chaosMarineModel = meq
  & model_name .~ "chaos marine"

chaosTerminatorModel :: Model
chaosTerminatorModel = meq
  & model_name .~ "chaos terminator"

plagueMarineModel :: Model
plagueMarineModel = meq
  & model_tgh  .~ 5
  & model_fnp  .~ 5
  & model_name .~ "plague marine"

blightlordTerminatorModel :: Model
blightlordTerminatorModel = teq
  & model_tgh  .~ 5
  & model_inv  .~ 4
  & model_fnp  .~ 5
  & model_name .~ "blightlord terminator"
