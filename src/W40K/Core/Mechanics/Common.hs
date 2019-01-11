{-# language RankNTypes #-}
{-# language RebindableSyntax #-}
{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Common where

import Prelude hiding (Functor(..), Monad(..), sequence)

import Data.List (sort)
import Data.Monoid ((<>))

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import Control.Lens

import Debug.Trace


data CombatType = Melee | Ranged deriving Eq


-- MODIFIERS & REROLLS

data IntMod = NoMod | ConstVal !Int | Add !Int | Times !Int | Half | Dot !IntMod !IntMod
  deriving (Eq, Ord, Show)

applyIntMod :: IntMod -> Int -> Int
applyIntMod NoMod        n = n
applyIntMod (ConstVal k) n = k
applyIntMod (Add k)      n = n + k
applyIntMod (Times k)    n = n * k
applyIntMod Half         n = (n+1) `div` 2
applyIntMod (Dot m1 m2)  n = applyIntMod m1 (applyIntMod m2 n)

instance Semigroup IntMod where
    (<>) = Dot
instance Monoid IntMod where
    mempty = NoMod

data Reroll = NoReroll | RerollOnes | RerollFailed | RerollAll
  deriving (Eq, Ord, Show)

instance Semigroup Reroll where
    (<>) = max
instance Monoid Reroll where
    mempty = NoReroll


data RollMods = RollMods
  { _mod_tohit     :: !Int
  , _mod_rrtohit   :: !Reroll
  , _mod_towound   :: !Int
  , _mod_rrtowound :: !Reroll
  , _mod_toarmor   :: !Int
  , _mod_rrarmor   :: !Reroll
  , _mod_toinv     :: !Int
  , _mod_rrinv     :: !Reroll
  , _mod_tobehit   :: !Int
  , _mod_recvdmg   :: !IntMod
  }
  deriving (Eq, Ord, Show)

makeLenses ''RollMods

noMods :: RollMods
noMods = RollMods 0 NoReroll 0 NoReroll 0 NoReroll 0 NoReroll 0 NoMod

mod_tosave :: Setter' RollMods Int
mod_tosave f mods =
    (\toarmor toinv -> mods { _mod_toarmor = toarmor, _mod_toinv = toinv })
      <$> f (_mod_toarmor mods) <*> f (_mod_toinv mods)

mod_rrtosave :: Setter' RollMods Reroll
mod_rrtosave f mods =
    (\rrarmor rrinv -> mods { _mod_rrarmor = rrarmor, _mod_rrinv = rrinv })
      <$> f (_mod_rrarmor mods) <*> f (_mod_rrinv mods)

instance Semigroup RollMods where
    m1 <> m2 = RollMods
      { _mod_tohit     = _mod_tohit m1     +  _mod_tohit m2
      , _mod_rrtohit   = _mod_rrtohit m1   <> _mod_rrtohit m2
      , _mod_towound   = _mod_towound m1   +  _mod_towound m2
      , _mod_rrtowound = _mod_rrtowound m1 <> _mod_rrtowound m2
      , _mod_toarmor   = _mod_toarmor m1   +  _mod_toarmor m2
      , _mod_rrarmor   = _mod_rrarmor m1   <> _mod_rrarmor m2
      , _mod_toinv     = _mod_toinv m1     +  _mod_toinv m2
      , _mod_rrinv     = _mod_rrinv m1     <> _mod_rrinv m2
      , _mod_tobehit   = _mod_tobehit m1   +  _mod_tobehit m2
      , _mod_recvdmg   = _mod_recvdmg m1   <> _mod_recvdmg m2
      }

instance Monoid RollMods where
    mempty = noMods

data Aura = Aura
  { _aura_cc  :: !RollMods
  , _aura_rng :: !RollMods
  }

makeLenses ''Aura

aura_any :: Setter' Aura RollMods
aura_any f (Aura cc rng) = Aura <$> f cc <*> f rng

noAura :: Aura
noAura = Aura noMods noMods

instance Semigroup Aura where
    a1 <> a2 = Aura
      { _aura_cc  = _aura_cc a1  <> _aura_cc a2
      , _aura_rng = _aura_rng a1 <> _aura_rng a2
      }
instance Monoid Aura where
    mempty = noAura


-- WEAPONS

data RequiredRoll = MinModifiedRoll !Int | MinUnmodifiedRoll !Int
  deriving (Eq, Ord, Show)

data RollHook eff = RollHook
  { _hook_minRoll :: !RequiredRoll
  , _hook_eff     :: !eff
  }
  deriving (Eq, Ord, Show)

makeLenses ''RollHook

data HitHookEff
    = HitHookExtraHits    !Int
    | HitHookExtraAttacks !Int
  deriving (Eq, Ord, Show)

type HitHook = RollHook HitHookEff

data WoundHookEff
    = WoundHookExtraAttacks !Int
    | WoundHookExtraHits    !Int
    | WoundHookExtraWounds  !Int
    | WoundHookMortalWounds !(Prob Int)
    | WoundHookMortalDamage !(Prob Int)
    | WoundHookModWeapon    !Weapon
  deriving (Eq, Ord, Show)

type WoundHook = RollHook WoundHookEff

data RollHooks = RollHooks
  { _hook_hit   :: ![HitHook]
  , _hook_wound :: ![WoundHook]
  , _hook_dmg   :: ()
  }
  deriving (Eq, Ord, Show)

noHooks :: RollHooks
noHooks = RollHooks [] [] ()

addHook :: RequiredRoll -> eff -> [RollHook eff] -> [RollHook eff]
addHook reqRoll eff = (RollHook reqRoll eff:)

data ModelClass = Infantry | Monster | Vehicle | Swarm | Cavalry | Biker | Beast | Battlesuit
  deriving (Eq, Ord, Show)

data WeaponWoundingMode = UseStrength | FixedWoundingAgainst ![ModelClass] !Int
  deriving (Eq, Ord, Show)

data Weapon = Weapon
  { _w_ap       :: !Int
  , _w_dmg      :: !(Prob Int)
  , _w_mods     :: !RollMods
  , _w_autohit  :: !Bool
  , _w_noinv    :: !Bool
  , _w_wounding :: !WeaponWoundingMode
  , _w_hooks    :: !RollHooks
  , _w_name     :: !String
  }
  deriving (Eq, Ord, Show)


makeLenses ''RollHooks
makeLenses ''Weapon

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


-- SPECIALIZED WEAPONS

class IsWeapon w where
    as_weapon :: Lens' w Weapon
    weaponAttacks :: Model -> w -> Prob Int

    hitMods :: Model -> w -> Model -> Int
    hitRoll :: Model -> w -> Model -> Prob Int
    doesHit :: Model -> w -> Model -> Int -> Bool
    -- probHit :: Model -> w -> Model -> QQ

    woundMods :: Model -> w -> Int
    woundRoll :: Model -> w -> Model -> Prob Int
    doesWound :: Model -> w -> Model -> Int -> Bool
    -- probWound :: Model -> w -> Model -> QQ

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


-- MODELS

data Model = Model
  { _model_class            :: !ModelClass
  , _model_ws               :: !Int
  , _model_bs               :: !Int
  , _model_str              :: !Int
  , _model_tgh              :: !Int
  , _model_att              :: !Int
  , _model_wnd              :: !Int
  , _model_ld               :: !Int
  , _model_save             :: !Int
  , _model_cc_inv           :: !Int
  , _model_rng_inv          :: !Int
  , _model_cc_mods          :: !RollMods
  , _model_rng_mods         :: !RollMods
  , _model_moved            :: !Bool
  , _model_quantumShielding :: !Bool
  , _model_allIsDust        :: !Bool
  , _model_ignoreHeavy      :: !Bool
  , _model_fnp              :: !Int
  , _model_name             :: !String
  }
  deriving (Eq, Ord, Show)

makeLenses ''Model

class IsModel m where
    as_model   :: Lens' m Model

instance IsModel Model where
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

-- model_aura :: Lens' Model Aura
-- model_aura f model =
--     rebuildModel <$> f (Aura (_model_cc_mods model) (_model_rng_mods model))
--   where
--     rebuildModel (Aura cc rng) = model { _model_cc_mods = cc, _model_rng_mods = rng }

model_mods_for :: CombatType -> Lens' Model RollMods
model_mods_for Melee  = model_cc_mods
model_mods_for Ranged = model_rng_mods


-- UTILS

(>=!) :: Int -> Int -> Bool
k >=! m = k > 1 && k >= m
infix 4 >=!

d3 :: Prob Int
d3 = uniformly [1..3]

d6 :: Prob Int
d6 = uniformly [1..6]

d6rr1 :: Prob Int
d6rr1 = do
    r <- d6
    if r == 1 then d6 else return r

prob_d6_gt :: Int -> QQ
prob_d6_gt k
  | k > 6     = 0
  | k < 2     = 1
  | otherwise = (7 - fromIntegral k) / 6

roll :: Reroll -> Prob Int -> (Int -> Bool) -> (Int -> Bool) -> Prob Bool
roll rr d noModPass modPass = do
    k <- d

    if modPass k then
      return True
    else if rerollable k then
      fmap modPass d
    else
      return False
  where
    rerollable k =
      case rr of
        RerollOnes   | k == 1            -> True
        RerollFailed | not (noModPass k) -> True
        RerollAll                        -> True
        otherwise                        -> False


skillRoll :: Reroll -> Int -> Int -> Prob Int
skillRoll rr skill mods = do
    k <- d6

    let modK = k + mods

    if k == 1 then
      if rerollable k then
        skillRoll NoReroll skill mods
      else
        return 1
    else
      if modK >= skill then
        return modK
      else if rerollable k then
        skillRoll NoReroll skill mods
      else
        return (max 1 modK)
  where
    rerollable k =
      case rr of
        RerollOnes   | k == 1    -> True
        RerollFailed | k < skill -> True
        RerollAll                -> True
        otherwise                -> False


data ChargeRerolls = NoChargeReroll | RerollChargeOneDie | RerollChargeAllDice | RerollChargeAnyDice

chargeRoll :: ChargeRerolls -> Int -> Prob Bool
chargeRoll rr minRoll = do
  [a,b] <- fmapProb sort $ sequence [d6,d6]
  let dist = a + b

  if dist >= minRoll then
    return True
  else
    case rr of
      NoChargeReroll      -> return False
      RerollChargeOneDie  -> bernoulli (prob_d6_gt (minRoll-b))
      RerollChargeAllDice -> bernoulli baseProbPass
      RerollChargeAnyDice
        | baseProbPass >= prob_d6_gt (minRoll-b) -> bernoulli baseProbPass
        | otherwise                              -> bernoulli (prob_d6_gt (minRoll-b))
  where
    baseProbPass = probTrue $ chargeRoll NoChargeReroll minRoll



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

