{-# language RankNTypes #-}
module W40K.Data.Inquisition where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import W40K.Core.Mechanics

import W40K.Data.Common
import qualified W40K.Data.Marines as Marines


-- MODIFIERS

data Quarry = OrdoMalleus | OrdoHereticus | OrdoXenos | OrdoSpecialist
    deriving (Eq, Ord, Show)

quarryBonus :: ModelEffect
quarryBonus = as_model.model_mods %~ stack [mod_rrtohit <>~ RerollFailed, mod_rrtowound <>~ RerollFailed]
-- TODO: keyword verification


-- MODELS

landraiderPrometheusModel :: Model
landraiderPrometheusModel = Marines.landraider
  & model_name .~ "landraider prometheus"


-- EQUIPPED MODELS

landraiderPrometheusWith :: [RngWeapon] -> EquippedModel
landraiderPrometheusWith rw = equipped landraiderPrometheusModel
  & em_rw   .~ rw
  & em_name .~ "LR prometheus w/ " ++ weaponNames rw

landraiderPrometheusPlus :: [RngWeapon] -> EquippedModel
landraiderPrometheusPlus rw = equipped landraiderPrometheusModel
  & em_rw   .~ rw ++ two [quad heavyBolter]
  & em_name .~ "LR prometheus w/ bolters+" ++ weaponNames rw

landraiderPrometheus :: EquippedModel
landraiderPrometheus = landraiderPrometheusPlus []
