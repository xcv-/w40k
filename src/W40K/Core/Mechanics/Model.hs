{-# language RankNTypes #-}
{-# language StrictData #-}
{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Model where

import Control.Lens

import W40K.Core.Mechanics.Roll


data ModelClass = Infantry | Monster | Vehicle | Swarm | Cavalry | Biker | Beast | Battlesuit
  deriving (Eq, Ord, Show)


data Model = Model
  { _model_class              :: ModelClass
  , _model_ws                 :: Int
  , _model_bs                 :: Int
  , _model_str                :: Int
  , _model_tgh                :: Int
  , _model_att                :: Int
  , _model_wnd                :: Int
  , _model_ld                 :: Int
  , _model_save               :: Int
  , _model_cc_inv             :: Int
  , _model_rng_inv            :: Int
  , _model_cc_mods            :: RollMods
  , _model_rng_mods           :: RollMods
  , _model_moved              :: Bool
  , _model_quantumShielding   :: Bool
  , _model_allIsDust          :: Bool
  , _model_unmodifiedMinWound :: Int
  , _model_ignoreHeavy        :: Bool
  , _model_fnp                :: Int
  , _model_name               :: String
  }
  deriving (Eq, Ord, Show)

makeLenses ''Model


class AsModel m where
    as_model   :: Lens' m Model

instance AsModel Model where
    as_model = id


model_inv :: Setter' Model Int
model_inv f model =
    (\ccinv rnginv -> model { _model_cc_inv = ccinv, _model_rng_inv = rnginv })
    <$>
    f (_model_cc_inv model)
    <*>
    f (_model_rng_inv model)

model_mods :: Setter' Model RollMods
model_mods f model =
    (\ccmods rngmods -> model { _model_cc_mods = ccmods, _model_rng_mods = rngmods })
    <$>
    f (_model_cc_mods model)
    <*>
    f (_model_rng_mods model)


data CombatType = Melee | Ranged deriving Eq

model_mods_for :: CombatType -> Lens' Model RollMods
model_mods_for Melee  = model_cc_mods
model_mods_for Ranged = model_rng_mods


data UnitOf m = UnitOf { unitSize :: Int, unitMember :: m }

instance AsModel m => AsModel (UnitOf m) where
    as_model = single.as_model
      where
        single f (UnitOf n m) = fmap (UnitOf n) (f m)


type ModelEffect = forall m. AsModel m => m -> m


applyAura :: Aura -> ModelEffect
applyAura aura = as_model %~
    (model_cc_mods  <>~ aura^.aura_cc) . (model_rng_mods <>~ aura^.aura_rng)


moving :: ModelEffect
moving = as_model.model_moved .~ True
