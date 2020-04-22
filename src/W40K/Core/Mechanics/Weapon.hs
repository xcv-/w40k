{-# language RankNTypes #-}
{-# language RebindableSyntax #-}
{-# language StrictData #-}
{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Weapon where

import Prelude hiding (Functor(..), Monad(..), sequence)

import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics.Roll
import W40K.Core.Mechanics.Model


-- Hooks: 6+ to hit, 4+ to wound, etc

data RequiredRoll = MinModifiedRoll Int | MinUnmodifiedRoll Int
  deriving (Eq, Ord, Show)

satisfiesReqRoll :: RequiredRoll -> SkillRoll -> Bool
satisfiesReqRoll (MinModifiedRoll m)   r = modifiedRoll r >= m
satisfiesReqRoll (MinUnmodifiedRoll m) r = unmodifiedRoll r >= m


data RollHook eff = RollHook
  { _hook_minRoll :: RequiredRoll
  , _hook_eff     :: eff
  }
  deriving (Eq, Ord, Show)



data HitHookEff
    = HitHookExtraHits    Int
    | HitHookExtraAttacks Int
  deriving (Eq, Ord, Show)

type HitHook = RollHook HitHookEff


data WoundHookEff
    = WoundHookExtraAttacks Int
    | WoundHookExtraHits    Int
    | WoundHookExtraWounds  Int
    | WoundHookMortalWounds (Prob Int)
    | WoundHookMortalDamage (Prob Int)
    | WoundHookModWeapon    Weapon
  deriving (Eq, Ord, Show)

type WoundHook = RollHook WoundHookEff


newtype DmgHook
    = DmgHookMortalWounds (Prob Int)
  deriving (Eq, Ord, Show)


data RollHooks = RollHooks
  { _hook_hit   :: [HitHook]
  , _hook_wound :: [WoundHook]
  , _hook_dmg   :: Maybe DmgHook
  }
  deriving (Eq, Ord, Show)

noHooks :: RollHooks
noHooks = RollHooks [] [] Nothing

addHook :: RequiredRoll -> eff -> [RollHook eff] -> [RollHook eff]
addHook reqRoll eff = (RollHook reqRoll eff:)


data WeaponWoundingMode
    = UseStrength                           -- strength/toughness table
    | FixedWoundingAgainst [ModelClass] Int -- poison-like
  deriving (Eq, Ord, Show)


data Weapon = Weapon
  { _w_ap       :: Int
  , _w_dmg      :: Prob Int
  , _w_mods     :: RollMods
  , _w_autohit  :: Bool
  , _w_noinv    :: Bool
  , _w_wounding :: WeaponWoundingMode
  , _w_hooks    :: RollHooks
  , _w_name     :: String
  }
  deriving (Eq, Ord, Show)


basicWeapon :: String -> Weapon
basicWeapon name = Weapon
  { _w_ap       = 0
  , _w_dmg      = return 1
  , _w_mods     = noMods
  , _w_autohit  = False
  , _w_noinv    = False
  , _w_wounding = UseStrength
  , _w_hooks    = noHooks
  , _w_name     = name
  }


makeLenses ''RollHook -- placed here for TH slicing/scope reasons
makeLenses ''RollHooks
makeLenses ''Weapon


-- Concrete weapon types: see Weapon.Melee, Weapon.Ranged

-- Project the basic weapon profile

class AsWeapon w where
    as_weapon :: Lens' w Weapon

instance AsWeapon Weapon where
    as_weapon = id


-- Specialized hit/wound/saves for melee and ranged combat

class AsWeapon w => IsWeapon w where
    weaponAttacks :: Model -> w -> Prob Int

    hitMods :: Model -> w -> Model -> Int
    hitRoll :: Model -> w -> Model -> Prob SkillRoll
    doesHit :: Model -> w -> Model -> SkillRoll -> Bool

    woundMods :: Model -> w -> Int
    woundRoll :: Model -> w -> Model -> Prob SkillRoll
    doesWound :: Model -> w -> Model -> SkillRoll -> Bool

    probSave :: w -> Model -> QQ

    shrinkModels :: [(Model, w)] -> [(Int, Model, w)]


probHit :: IsWeapon w => Model -> w -> Model -> QQ
probHit src w tgt
  | w^.as_weapon.w_autohit = 1
  | otherwise              = probOf (doesHit src w tgt) (hitRoll src w tgt)


probWound :: IsWeapon w => Model -> w -> Model -> QQ
probWound src w tgt = probOf (doesWound src w tgt) (woundRoll src w tgt)


probFailSave :: IsWeapon w => w -> Model -> QQ
probFailSave w tgt = 1 - probSave w tgt


-- utility function
allIsDustSaveMod :: Weapon -> Model -> Int
allIsDustSaveMod w tgt
  | tgt^.model_allIsDust && (w^.w_dmg == return 1) = 1
  | otherwise                                      = 0


requiredWoundRoll :: Weapon -> Int -> Model -> Int
requiredWoundRoll w str tgt =
    case w^.w_wounding of
      UseStrength -> compareStrTgh str (tgt^.model_tgh)

      FixedWoundingAgainst cls minWndRoll
        | tgt^.model_class `elem` cls -> minWndRoll
        | otherwise                   -> compareStrTgh str (tgt^.model_tgh)
  where
    compareStrTgh str tgh
      | 2*str <=   tgh = 6
      |   str >= 2*tgh = 2
      |   str <    tgh = 5
      |   str >    tgh = 3
      | otherwise      = 4
