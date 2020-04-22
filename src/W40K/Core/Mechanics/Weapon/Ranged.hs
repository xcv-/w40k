{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Weapon.Ranged
  ( RngWeaponClass (..)
  , RngWeapon (..)
  , rw_shots, rw_str, rw_class, rw_weapon, rw_melta
  , rw_ap, rw_dmg, rw_mods, rw_name
  , null_rw
  , module Weapon
  ) where

import Prelude hiding (Functor(..), Monad(..))
import Data.Monoid ((<>))
import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics.Model
import W40K.Core.Mechanics.Roll
import W40K.Core.Mechanics.Weapon as Weapon
import W40K.Core.Util (groupWith)

data RngWeaponClass = Heavy | Grenade | RapidFire | Pistol | Assault
  deriving (Eq, Ord, Show)

data RngWeapon = RngWeapon
  { _rw_shots   :: Prob Int
  , _rw_str     :: Int
  , _rw_class   :: RngWeaponClass
  , _rw_weapon  :: Weapon
  , _rw_melta   :: Bool
  }
  deriving (Eq, Ord, Show)

makeLenses ''RngWeapon

rw_ap :: Lens' RngWeapon Int
rw_ap  = rw_weapon.w_ap

rw_dmg :: Lens' RngWeapon (Prob Int)
rw_dmg = rw_weapon.w_dmg

rw_mods :: Lens' RngWeapon RollMods
rw_mods = rw_weapon.w_mods

rw_name :: Lens' RngWeapon String
rw_name = rw_weapon.w_name

null_rw :: RngWeapon
null_rw = RngWeapon
  { _rw_shots   = return 0
  , _rw_str     = 0
  , _rw_class   = Assault
  , _rw_melta   = False
  , _rw_weapon  = basicWeapon "(null)"
  }

instance AsWeapon RngWeapon where
    as_weapon = rw_weapon

instance IsWeapon RngWeapon where
    weaponAttacks _ w = w^.rw_shots

    hitMods = rngHitMods
    hitRoll = rngHitRoll
    doesHit = rngDoesHit
    -- probHit = rngProbHit

    woundMods = rngWoundMods
    woundRoll = rngWoundRoll
    doesWound = rngDoesWound
    -- probWound = rngProbWound

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
      | w^.rw_class == Heavy && src^.model_moved && not (src^.model_ignoreHeavy) = -1
      | otherwise                                                                = 0

-- autohit not taken into account because there is no hit roll for auto-hitting weapons (FAQ'd)
rngDoesHit :: Model -> RngWeapon -> Model -> SkillRoll -> Bool
rngDoesHit src _ _ =
    (>=! src^.model_bs) . modifiedRoll

rngHitRoll :: Model -> RngWeapon -> Model -> Prob SkillRoll
rngHitRoll src w tgt =
    skillRoll rerolls 1 (src^.model_bs) (rngHitMods src w tgt)
  where
    rerolls = src^.model_rng_mods.mod_rrtohit <> w^.rw_mods.mod_rrtohit


rngWoundMods :: Model -> RngWeapon -> Int
rngWoundMods src w = src^.model_rng_mods.mod_towound + w^.rw_mods.mod_towound

rngDoesWound :: Model -> RngWeapon -> Model -> SkillRoll -> Bool
rngDoesWound _ w tgt r =
    unmodifiedRoll r >=! tgt^.model_unmodifiedMinWound && modifiedRoll r >=! requiredRoll
  where
    requiredRoll = requiredWoundRoll (w^.rw_weapon) (w^.rw_str) tgt

rngWoundRoll :: Model -> RngWeapon -> Model -> Prob SkillRoll
rngWoundRoll src w tgt =
    skillRoll rerolls (tgt^.model_unmodifiedMinWound) requiredRoll (rngWoundMods src w)
  where
    rerolls = src^.model_rng_mods.mod_rrtowound <> w^.rw_mods.mod_rrtowound

    requiredRoll = requiredWoundRoll (w^.rw_weapon) (w^.rw_str) tgt


rngSaveArmor :: RngWeapon -> Model -> Prob Bool
rngSaveArmor rw tgt =
    roll reroll d6 (>=! requiredRoll) (>=! requiredRoll - armorMod)
  where
    w = rw^.rw_weapon
    requiredRoll = tgt^.model_save
    armorMod     = tgt^.model_rng_mods.mod_toarmor + w^.w_ap + allIsDustSaveMod (rw^.as_weapon) tgt
    reroll       = tgt^.model_rng_mods.mod_rrarmor

rngSaveInv :: RngWeapon -> Model -> Prob Bool
rngSaveInv rw tgt
  | w^.w_noinv = return False
  | otherwise  = roll reroll d6 (>=! requiredRoll) (>=! requiredRoll - invMod)
  where
    w = rw^.rw_weapon
    requiredRoll = tgt^.model_rng_inv
    invMod       = tgt^.model_rng_mods.mod_toinv + allIsDustSaveMod (rw^.as_weapon) tgt
    reroll       = tgt^.model_rng_mods.mod_rrinv

rngProbSave :: RngWeapon -> Model -> QQ
rngProbSave rw tgt =
    max (probTrue (rngSaveArmor rw tgt))
        (probTrue (rngSaveInv   rw tgt))


rngShrinkModels :: [(Model, RngWeapon)] -> [(Int, Model, RngWeapon)]
rngShrinkModels = groupWith eqrel (\(m,rw) mrws -> (1 + length mrws, m, rw))
  where
    relevantModelFields m = (m^.model_bs, m^.model_rng_mods, m^.model_moved && not (m^.model_ignoreHeavy))

    eqrel :: (Model, RngWeapon) -> (Model, RngWeapon) -> Bool
    eqrel (m1,w1) (m2,w2) = relevantModelFields m1 == relevantModelFields m2 && w1 == w2

    -- sumShots (src, w) srcws = (src, w & rw_shots %~ liftA2 (+) (numShots srcws))
    -- numShots = sumProbs . map (^._2.rw_shots)
