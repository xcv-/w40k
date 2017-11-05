{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Melee
  ( CCWeapon (..)
  , ccw_strMod, ccw_unwieldly, ccw_attBonus, ccw_weapon
  , ccw_ap, ccw_dmg, ccw_mods, ccw_name
  , basic_ccw
  ) where

import Prelude hiding (Functor(..), Monad(..))
import Data.Monoid ((<>))
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics.Common

data CCWeapon = CCWeapon
  { _ccw_strMod    :: IntMod
  , _ccw_unwieldly :: Bool
  , _ccw_attBonus  :: IntMod
  , _ccw_weapon    :: Weapon
  }
  deriving Eq

makeLenses ''CCWeapon

ccw_ap :: Lens' CCWeapon Int
ccw_ap = ccw_weapon.w_ap

ccw_dmg :: Lens' CCWeapon (Prob Int)
ccw_dmg = ccw_weapon.w_dmg

ccw_mods :: Lens' CCWeapon RollMods
ccw_mods = ccw_weapon.w_mods

ccw_name :: Lens' CCWeapon String
ccw_name = ccw_weapon.w_name

basic_ccw :: CCWeapon
basic_ccw = CCWeapon
  { _ccw_strMod    = NoMod
  , _ccw_unwieldly = False
  , _ccw_attBonus  = NoMod
  , _ccw_weapon    = basicWeapon "default ccw"
  }


instance IsWeapon CCWeapon where
    as_weapon = ccw_weapon
    weaponAttacks src w = return $ applyIntMod (w^.ccw_attBonus) (src^.model_att)

    hitMods = ccHitMods
    doesHit = ccDoesHit
    probHit = ccProbHit

    woundMods = ccWoundMods
    doesWound = ccDoesWound
    probWound = ccProbWound

    probSave = ccProbSave

    shrinkModels = ccShrinkModels


ccHitMods :: Model -> CCWeapon -> Model -> Int
ccHitMods src w tgt =
     src^.model_cc_mods.mod_tohit
     + w^.ccw_mods.mod_tohit
     + tgt^.model_cc_mods.mod_tobehit

ccDoesHit :: Model -> CCWeapon -> Model -> Int -> Bool
ccDoesHit src w tgt = (>=! src^.model_ws - ccHitMods src w tgt)

ccHitRoll :: Model -> CCWeapon -> Model -> Prob Bool
ccHitRoll src w tgt =
    roll rerolls d6 (>=! src^.model_ws) (ccDoesHit src w tgt)
  where
    rerolls = src^.model_cc_mods.mod_rrtohit <> w^.ccw_mods.mod_rrtohit

ccProbHit :: Model -> CCWeapon -> Model -> QQ
ccProbHit src w tgt = probTrue (ccHitRoll src w tgt)


ccWoundMods :: Model -> CCWeapon -> Int
ccWoundMods src w = src^.model_cc_mods.mod_towound + w^.ccw_mods.mod_towound

ccDoesWound :: Model -> CCWeapon -> Model -> Int -> Bool
ccDoesWound src w tgt =
    (>=! requiredRoll - ccWoundMods src w)
  where
    requiredRoll = requiredWoundRoll (w^.ccw_weapon)
                                     (applyIntMod (w^.ccw_strMod) $ src^.model_str)
                                     (tgt^.model_tgh)

ccWoundRoll :: Model -> CCWeapon -> Model -> Prob Bool
ccWoundRoll src w tgt =
    roll rerolls d6 (>=! requiredRoll) (ccDoesWound src w tgt)
  where
    rerolls = src^.model_cc_mods.mod_rrtowound <> w^.ccw_mods.mod_rrtowound

    requiredRoll = requiredWoundRoll (w^.ccw_weapon)
                                     (applyIntMod (w^.ccw_strMod) (src^.model_str))
                                     (tgt^.model_tgh)

ccProbWound :: Model -> CCWeapon -> Model -> QQ
ccProbWound src w tgt = probTrue (ccWoundRoll src w tgt)

ccSaveArmor :: CCWeapon -> Model -> Prob Bool
ccSaveArmor ccw tgt =
    roll reroll d6 (>=! requiredRoll) (>=! requiredRoll - armorMod)
  where
    w = ccw^.ccw_weapon
    requiredRoll = tgt^.model_save
    armorMod     = tgt^.model_cc_mods.mod_toarmor + w^.w_ap
    reroll       = tgt^.model_cc_mods.mod_rrarmor

ccSaveInv :: CCWeapon -> Model -> Prob Bool
ccSaveInv ccw tgt
  | w^.w_noinv = return False
  | otherwise  = roll reroll d6 (>=! requiredRoll) (>=! requiredRoll - invMod)
  where
    w = ccw^.ccw_weapon
    requiredRoll = tgt^.model_cc_inv
    invMod       = tgt^.model_cc_mods.mod_toinv
    reroll       = tgt^.model_cc_mods.mod_rrinv

ccProbSave :: CCWeapon -> Model -> QQ
ccProbSave ccw tgt =
    max (probTrue (ccSaveArmor ccw tgt))
        (probTrue (ccSaveInv   ccw tgt))


ccShrinkModels :: [(Model, CCWeapon)] -> [(Model, CCWeapon)]
ccShrinkModels = groupWith (==) sumAttacks
  where
    sumAttacks (src, w) srcws = (src, w & ccw_attBonus %~ Dot (Add (numAttacks srcws)))

    numAttacks = sum . map (\(src, w) -> applyIntMod (w^.ccw_attBonus) (src^.model_att))

