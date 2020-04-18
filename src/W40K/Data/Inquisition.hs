module W40K.Data.Inquisition where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import W40K.Core.Mechanics

import W40K.Data.Common
import qualified W40K.Data.Marines as Marines


-- MODIFIERS

data Quarry = OrdoMalleus | OrdoHereticus | OrdoXenos | OrdoSpecialist
    deriving (Eq, Ord, Show)

quarryBonus :: Quarry -> Model -> Model
quarryBonus OrdoMalleus    = stack [ model_mods.mod_rrtohit   <>~ RerollFailed
                                   , model_mods.mod_rrtowound <>~ RerollFailed
                                   ]
quarryBonus OrdoXenos      = stack [ model_mods.mod_rrtohit   <>~ RerollFailed
                                   , model_mods.mod_rrtowound <>~ RerollFailed
                                   ]
quarryBonus OrdoHereticus  = quarryBonus OrdoMalleus
quarryBonus OrdoSpecialist = quarryBonus OrdoXenos


-- MODELS

landraiderPrometheusModel :: Model
landraiderPrometheusModel = Marines.landraider
  & model_name .~ "landraider prometheus"


-- EQUIPPED MODELS

landraiderPrometheusWith :: [RngWeapon] -> EquippedModel
landraiderPrometheusWith rw = basicEquippedModel landraiderPrometheusModel
  & em_rw   .~ rw
  & em_name .~ "LR prometheus w/ " ++ weaponNames rw

landraiderPrometheusPlus :: [RngWeapon] -> EquippedModel
landraiderPrometheusPlus rw = basicEquippedModel landraiderPrometheusModel
  & em_rw   .~ rw ++ two [quad heavyBolter]
  & em_name .~ "LR prometheus w/ bolters+" ++ weaponNames rw

landraiderPrometheus :: EquippedModel
landraiderPrometheus = landraiderPrometheusPlus []
