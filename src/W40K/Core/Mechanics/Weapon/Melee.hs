{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Weapon.Melee
  ( CCWeapon (..)
  , ccw_strMod, ccw_attBonus, ccw_weapon
  , ccw_ap, ccw_dmg, ccw_mods, ccw_name
  , basic_ccw, makeUnwieldly
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


data CCWeapon = CCWeapon
  { _ccw_strMod    :: IntMod
  , _ccw_attBonus  :: IntMod
  , _ccw_weapon    :: Weapon
  }
  deriving (Eq, Ord, Show)

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
  , _ccw_attBonus  = NoMod
  , _ccw_weapon    = basicWeapon "default ccw"
  }

makeUnwieldly :: CCWeapon -> CCWeapon
makeUnwieldly = ccw_weapon.w_mods.mod_tohit -~ 1


instance AsWeapon CCWeapon where
    as_weapon = ccw_weapon


instance IsWeapon CCWeapon where
    weaponAttacks src w = return $ applyIntMod (w^.ccw_attBonus) (src^.model_att)

    hitMods = ccHitMods
    hitRoll = ccHitRoll
    doesHit = ccDoesHit
    -- probHit = ccProbHit

    woundMods = ccWoundMods
    woundRoll = ccWoundRoll
    doesWound = ccDoesWound
    -- probWound = ccProbWound

    probSave = ccProbSave

    shrinkModels = ccShrinkModels


ccHitMods :: Model -> CCWeapon -> Model -> Int
ccHitMods src w tgt =
     src^.model_cc_mods.mod_tohit
     + w^.ccw_mods.mod_tohit
     + tgt^.model_cc_mods.mod_tobehit

-- autohit not taken into account because there is no hit roll for auto-hitting weapons (FAQ'd)
ccDoesHit :: Model -> CCWeapon -> Model -> SkillRoll -> Bool
ccDoesHit src _ _ =
    (>=! src^.model_ws) . modifiedRoll

ccHitRoll :: Model -> CCWeapon -> Model -> Prob SkillRoll
ccHitRoll src w tgt =
    skillRoll rerolls 1 (src^.model_ws) (ccHitMods src w tgt)
  where
    rerolls = src^.model_cc_mods.mod_rrtohit <> w^.ccw_mods.mod_rrtohit


ccWoundMods :: Model -> CCWeapon -> Int
ccWoundMods src w = src^.model_cc_mods.mod_towound + w^.ccw_mods.mod_towound

ccDoesWound :: Model -> CCWeapon -> Model -> SkillRoll -> Bool
ccDoesWound src w tgt r =
    unmodifiedRoll r >=! tgt^.model_unmodifiedMinWound && modifiedRoll r >=! requiredRoll
  where
    requiredRoll = requiredWoundRoll (w^.ccw_weapon)
                                     (applyIntMod (w^.ccw_strMod) $ src^.model_str)
                                     tgt

ccWoundRoll :: Model -> CCWeapon -> Model -> Prob SkillRoll
ccWoundRoll src w tgt =
    skillRoll rerolls (tgt^.model_unmodifiedMinWound) requiredRoll (ccWoundMods src w)
  where
    rerolls = src^.model_cc_mods.mod_rrtowound <> w^.ccw_mods.mod_rrtowound

    requiredRoll = requiredWoundRoll (w^.ccw_weapon)
                                     (applyIntMod (w^.ccw_strMod) (src^.model_str))
                                     tgt


ccSaveArmor :: CCWeapon -> Model -> Prob Bool
ccSaveArmor ccw tgt =
    roll reroll d6 (>=! requiredRoll) (>=! requiredRoll - armorMod)
  where
    w = ccw^.ccw_weapon
    requiredRoll = tgt^.model_save
    armorMod     = tgt^.model_cc_mods.mod_toarmor + w^.w_ap + allIsDustSaveMod (ccw^.as_weapon) tgt
    reroll       = tgt^.model_cc_mods.mod_rrarmor

ccSaveInv :: CCWeapon -> Model -> Prob Bool
ccSaveInv ccw tgt
  | w^.w_noinv = return False
  | otherwise  = roll reroll d6 (>=! requiredRoll) (>=! requiredRoll - invMod)
  where
    w = ccw^.ccw_weapon
    requiredRoll = tgt^.model_cc_inv
    invMod       = tgt^.model_cc_mods.mod_toinv + allIsDustSaveMod (ccw^.as_weapon) tgt
    reroll       = tgt^.model_cc_mods.mod_rrinv

ccProbSave :: CCWeapon -> Model -> QQ
ccProbSave ccw tgt =
    max (probTrue (ccSaveArmor ccw tgt))
        (probTrue (ccSaveInv   ccw tgt))


ccShrinkModels :: [(Model, CCWeapon)] -> [(Int, Model, CCWeapon)]
ccShrinkModels = groupWith eqrel (\(m,ccw) mccws -> (1 + length mccws, m, ccw))
  where
    relevantModelFields m = (m^.model_ws, m^.model_att, m^.model_str, m^.model_cc_mods)
    -- relevantModelFields m = (m^.model_ws, m^.model_str, m^.model_cc_mods)

    eqrel :: (Model, CCWeapon) -> (Model, CCWeapon) -> Bool
    eqrel (m1,w1) (m2,w2) = relevantModelFields m1 == relevantModelFields m2 && w1 == w2

    -- sumAttacks (src, w) srcws = (src, w & ccw_attBonus %~ Dot (Add (numAttacks srcws)))
    -- numAttacks = sum . map (\(src, w) -> applyIntMod (w^.ccw_attBonus) (src^.model_att))

