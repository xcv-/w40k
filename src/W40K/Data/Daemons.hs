module W40K.Data.Daemons where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.Mechanics

import W40K.Data.Common


-- STRATAGEMS

cloudOfFlies :: Model -> Model
cloudOfFlies m = m
  & model_mods.mod_tobehit -~ 1
  & model_name             <>~ " (CoF)"

miasmaOfPestilence :: Model -> Model
miasmaOfPestilence m = m
  & model_mods.mod_tobehit -~ 1
  & model_name             <>~ " (MoP)"

warpSurge :: Model -> Model
warpSurge m = m
  & model_inv   %~ max 4 . subtract 1
  & model_name <>~ " (WS)"


-- MODELS

plagueBearer :: Model
plagueBearer = meq
  & model_ws   .~ 4
  & model_bs   .~ 4
  & model_save .~ 6
  & model_inv  .~ 5
  & model_fnp  .~ 5
  & model_name .~ "plaguebearer"
