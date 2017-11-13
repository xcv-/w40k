module W40K.Data.Chaos where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics

import W40K.Data.Common


allIsDust :: Model -> Model
allIsDust = (model_save -~ 1) . (model_inv -~ 1)

-- MODELS

daemonPrince :: Model
daemonPrince = meq
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
  & model_name  .~ "daemon prince"

tzaangor :: Model
tzaangor = meq
  & model_bs   .~ 4
  & model_ld   .~ 7
  & model_save .~ nosave
  & model_inv  .~ 5
  & model_name .~ "tzaangor"

rubricMarine :: Model
rubricMarine = meq
  & model_ld   .~ 7
  & model_inv  .~ 5
  & model_name .~ "rubric marine"

-- RANGED WEAPONS

infernoBolter :: RngWeapon
infernoBolter = bolter
  & rw_ap   .~ -2
  & rw_name .~ "inferno boltgun"

infernoBoltPistol :: RngWeapon
infernoBoltPistol = boltPistol
  & rw_ap .~ -2
  & rw_name .~ "inferno bolt pistol"


-- EQUIPPED MODELS

bolterRubricMarine :: EquippedModel
bolterRubricMarine = basicEquippedModel rubricMarine
  & em_rw .~ infernoBolter

aspiringSorcerer :: EquippedModel
aspiringSorcerer = basicEquippedModel rubricMarine
  & em_model %~ (model_att +~ 1) . (model_ld +~ 1)
  & em_ccw   .~ forceSword
  & em_rw    .~ boltPistol


-- SQUADS

rubricSquad :: Int -> [EquippedModel]
rubricSquad n = aspiringSorcerer : replicate (n-1) bolterRubricMarine
