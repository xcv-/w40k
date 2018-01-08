{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Ranged
  ( RngWeaponClass (..)
  , RngWeapon (..)
  , rw_shots, rw_str, rw_class, rw_autohit, rw_weapon, rw_melta
  , rw_ap, rw_dmg, rw_mods, rw_name, rw_points
  , null_rw
  ) where

import Prelude hiding (Functor(..), Monad(..))
import Data.Monoid ((<>))
import Control.Lens

import Debug.Trace

import W40K.Core.Prob
import W40K.Core.Mechanics.Common

data RngWeaponClass = Heavy | Grenade | RapidFire | Pistol | Assault deriving Eq

data RngWeapon = RngWeapon
  { _rw_shots   :: Prob Int
  , _rw_str     :: Int
  , _rw_class   :: RngWeaponClass
  , _rw_autohit :: Bool
  , _rw_weapon  :: Weapon
  , _rw_melta   :: Bool
  }
  deriving Eq

makeLenses ''RngWeapon

rw_ap :: Lens' RngWeapon Int
rw_ap  = rw_weapon.w_ap

rw_dmg :: Lens' RngWeapon (Prob Int)
rw_dmg = rw_weapon.w_dmg

rw_mods :: Lens' RngWeapon RollMods
rw_mods = rw_weapon.w_mods

rw_name :: Lens' RngWeapon String
rw_name = rw_weapon.w_name

rw_points :: Lens' RngWeapon Int
rw_points = rw_weapon.w_points

null_rw :: RngWeapon
null_rw = RngWeapon
  { _rw_shots   = return 0
  , _rw_str     = 0
  , _rw_class   = Assault
  , _rw_autohit = False
  , _rw_melta   = False
  , _rw_weapon  = basicWeapon "(null)"
  }

instance IsWeapon RngWeapon where
    as_weapon = rw_weapon
    weaponAttacks _ w = w^.rw_shots

    hitMods = rngHitMods
    doesHit = rngDoesHit
    probHit = rngProbHit

    woundMods = rngWoundMods
    doesWound = rngDoesWound
    probWound = rngProbWound

    probSave = rngProbSave

    shrinkModels = rngShrinkModels


rngHitMods :: Model -> RngWeapon -> Model -> Int
rngHitMods src w tgt =
    heavyWeaponMod
    + src^.model_rng_mods.mod_tohit
    + w^.rw_mods.mod_tohit
    + tgt^.model_rng_mods.mod_tobehit
  where
    heavyWeaponMod
      | w^.rw_class == Heavy && src^.model_moved && not (src^.model_machineSpirit) = -1
      | otherwise                                                                  = 0

rngDoesHit :: Model -> RngWeapon -> Model -> Int -> Bool
rngDoesHit src w tgt
  | w^.rw_autohit = const True
  | otherwise     = (>=! src^.model_bs - rngHitMods src w tgt)

rngHitRoll :: Model -> RngWeapon -> Model -> Prob Bool
rngHitRoll src w tgt =
    roll rerolls d6 (>=! src^.model_bs) (rngDoesHit src w tgt)
  where
    rerolls = src^.model_rng_mods.mod_rrtohit <> w^.rw_mods.mod_rrtohit

rngProbHit :: Model -> RngWeapon -> Model -> QQ
rngProbHit src w tgt = probTrue (rngHitRoll src w tgt)


rngWoundMods :: Model -> RngWeapon -> Int
rngWoundMods src w = src^.model_rng_mods.mod_towound + w^.rw_mods.mod_towound

rngDoesWound :: Model -> RngWeapon -> Model -> Int -> Bool
rngDoesWound src w tgt =
    (>=! requiredRoll - rngWoundMods src w)
  where
    requiredRoll = requiredWoundRoll (w^.rw_weapon) (w^.rw_str) tgt

rngWoundRoll :: Model -> RngWeapon -> Model -> Prob Bool
rngWoundRoll src w tgt =
    roll rerolls d6 (>=! requiredRoll) (rngDoesWound src w tgt)
  where
    rerolls = src^.model_rng_mods.mod_rrtowound <> w^.rw_mods.mod_rrtowound

    requiredRoll = requiredWoundRoll (w^.rw_weapon) (w^.rw_str) tgt

rngProbWound :: Model -> RngWeapon -> Model -> QQ
rngProbWound src w tgt = probTrue (rngWoundRoll src w tgt)


rngSaveArmor :: RngWeapon -> Model -> Prob Bool
rngSaveArmor rw tgt =
    roll reroll d6 (>=! requiredRoll) (>=! requiredRoll - armorMod)
  where
    w = rw^.rw_weapon
    requiredRoll = tgt^.model_save
    armorMod     = tgt^.model_rng_mods.mod_toarmor + w^.w_ap
    reroll       = tgt^.model_rng_mods.mod_rrarmor

rngSaveInv :: RngWeapon -> Model -> Prob Bool
rngSaveInv rw tgt
  | w^.w_noinv = return False
  | otherwise  = roll reroll d6 (>=! requiredRoll) (>=! requiredRoll - invMod)
  where
    w = rw^.rw_weapon
    requiredRoll = tgt^.model_rng_inv
    invMod       = tgt^.model_rng_mods.mod_toinv
    reroll       = tgt^.model_rng_mods.mod_rrinv

rngProbSave :: RngWeapon -> Model -> QQ
rngProbSave rw tgt =
    max (probTrue (rngSaveArmor rw tgt))
        (probTrue (rngSaveInv   rw tgt))

-- rngUnsavedWounds :: Model -> RngWeapon -> Model -> Prob Int
-- rngUnsavedWounds src w tgt = do
--     natt    <- w^.rw_shots
--     nhits   <- binomial natt  $ probTrue $ rngHit src w tgt
--     nwounds <- binomial nhits $ probTrue $ rngWound src w tgt
--     binomial nwounds $ probFalse $ rngSave (w^.rw_weapon) tgt

rngShrinkModels :: [(Model, RngWeapon)] -> [(Model, RngWeapon)]
rngShrinkModels = groupWith eqrel sumShots
  where
    relevantModelFields m = (m^.model_bs, m^.model_rng_mods, m^.model_moved, m^.model_machineSpirit)

    eqrel :: (Model, RngWeapon) -> (Model, RngWeapon) -> Bool
    eqrel (m1,w1) (m2,w2) = relevantModelFields m1 == relevantModelFields m2 && w1 == w2

    sumShots (src, w) srcws = (src, w & rw_shots %~ liftA2 (+) (numShots srcws))

    numShots = sumProbs . map (^._2.rw_shots)
