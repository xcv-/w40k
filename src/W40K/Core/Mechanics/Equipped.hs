{-# language RankNTypes #-}
{-# language StrictData #-}
{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Equipped
  ( EquippedModel(..)
  , em_model, em_ccw, em_rw, em_name, em_weapons
  , equipped
  , splitAttacks

  , Effect
  , stack
  , with
  , applyAura
  , within
  , rapidFiring
  , meltaRange
  ) where

import Control.Lens

import W40K.Core.ConstrMonad (liftA2)
import W40K.Core.Prob

import W40K.Core.Mechanics.Model
import W40K.Core.Mechanics.Roll
import W40K.Core.Mechanics.Weapon.Melee
import W40K.Core.Mechanics.Weapon.Ranged
import W40K.Core.Util (filteredOn)


data EquippedModel = EquippedModel
  { _em_model    :: Model
  , _em_ccw      :: CCWeapon
  , _em_rw       :: [RngWeapon]
  }
  deriving (Eq, Ord, Show)

makeLenses ''EquippedModel


instance AsModel EquippedModel where
    as_model = em_model


em_name :: Lens' EquippedModel String
em_name = em_model.model_name


em_weapons :: Traversal' EquippedModel Weapon
em_weapons f (EquippedModel m ccw rw) =
    EquippedModel m <$> as_weapon f ccw <*> (traverse.as_weapon) f rw


equipped :: Model -> EquippedModel
equipped model = EquippedModel model basic_ccw []

-- view as_model . equipped == id


splitAttacks :: Int -> CCWeapon -> EquippedModel -> [EquippedModel]
splitAttacks natt ccw em
  | em^.em_model.model_att < natt = [em]
  | otherwise                     =
      [ em & em_model.model_att -~ natt
      , em & em_model.model_att .~ natt & em_ccw .~ ccw
      ]


type Effect = EquippedModel -> EquippedModel

stack :: [a -> a] -> a -> a
stack = foldr (.) id

with :: [Effect] -> [EquippedModel] -> [EquippedModel]
with = map . stack

within :: Aura -> [EquippedModel] -> [EquippedModel]
within = map . applyAura

rapidFiring :: Effect
rapidFiring =
    em_rw.mapped.filteredOn rw_class (== RapidFire).rw_shots %~ fmapProb (*2)

meltaRange :: Effect
meltaRange = em_rw.mapped %~ meltaRangeWeapon
  where
    meltaRangeWeapon rw
      | rw^.rw_melta = rw & rw_dmg %~ \dmg -> liftA2 max dmg dmg
      | otherwise    = rw
