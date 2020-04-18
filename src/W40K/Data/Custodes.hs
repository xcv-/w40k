module W40K.Data.Custodes where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens

import W40K.Core.Mechanics

import W40K.Data.Common
import W40K.Data.Marines (leviathanDreadnought)


-- BANNERS

vexillaMagnifica :: Model -> Model
vexillaMagnifica m = m
  & model_rng_mods.mod_tobehit -~ 1
  & model_name                 <>~ " (VM)"

-- RELICS

auricAquilas :: Model -> Model
auricAquilas m = m
  & model_inv  .~ 3
  & model_name <>~ " (AA)"

-- WARLORD TRAITS

radiantMantle :: Model -> Model
radiantMantle m = m
  & model_mods.mod_tobehit -~ -1
  & model_name             <>~ " (RM)"

superiorCreation :: Model -> Model
superiorCreation m = m
  & model_fnp  .~ 5
  & model_name <>~ " (SC)"


-- MODELS

custodes :: Model
custodes = meq
  & model_ws   .~ 2
  & model_bs   .~ 2
  & model_str  .~ 5
  & model_tgh  .~ 5
  & model_wnd  .~ 3
  & model_att  .~ 3
  & model_ld   .~ 8
  & model_save .~ 2
  & model_inv  .~ 4
  & model_name .~ "custodes"


vexilusPraetor :: Model
vexilusPraetor = custodes
  & model_wnd  .~ 5
  & model_att  .~ 4
  & model_name .~ "vexilus praetor"


vertusPraetor :: Model
vertusPraetor = custodes
  & model_class .~ Biker
  & model_tgh   .~ 6
  & model_wnd   .~ 4
  & model_att   .~ 4
  & model_ld    .~ 9
  & model_name  .~ "vertus praetor"


dawneagleCaptain :: Model
dawneagleCaptain = vertusPraetor
  & model_att              .~ 5
  & model_wnd              .~ 7
  & model_mods.mod_rrtohit .~ RerollOnes
  & model_name             .~ "dawneagle captain"


calladius :: Model
calladius = rhino
  & model_bs          .~ 2
  & model_wnd         .~ 14
  & model_ld          .~ 9
  & model_inv         .~ 5
  & model_ignoreHeavy .~ True
  & model_name        .~ "calladius"


pallas :: Model
pallas = calladius
  & model_str         .~ 5
  & model_tgh         .~ 6
  & model_wnd         .~ 8
  & model_att         .~ 2
  & model_name        .~ "pallas"


telemon :: Model
telemon = leviathanDreadnought
  & model_fnp  .~ 6
  & model_name .~ "telemon"
