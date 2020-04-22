{-# language RebindableSyntax #-}
{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Roll where

import Prelude hiding (Functor(..), Monad(..), sequence)

import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Prob


---- Basic roll utilities

-- natural 1's always fail
(>=!) :: Int -> Int -> Bool
k >=! m = k > 1 && k >= m
infix 4 >=!


-- D3 dice
d3 :: Prob Int
d3 = uniformly [1..3]


-- D6 dice
d6 :: Prob Int
d6 = uniformly [1..6]


-- 2D6, first always smaller (reduces space size)
sorted2d6 :: Prob (Int, Int)
sorted2d6 = do
    a <- d6
    b <- d6
    return (min a b, max a b)


-- 1D6, rerolling ones
d6rr1 :: Prob Int
d6rr1 = do
    r <- d6
    if r == 1 then d6 else return r


-- probability of D6 >= k
prob_d6_geq :: Int -> QQ
prob_d6_geq k
  | k > 6     = 0
  | k < 2     = 1
  | otherwise = (7 - fromIntegral k) / 6



---- Modifiers and re-rolls

-- Skill roll examples: hit rolls, wound rolls, save rolls, etc
data SkillRoll = SkillRoll
  { unmodifiedRoll :: {-# unpack #-} !Int
  , rollModifiers  :: {-# unpack #-} !Int
  }
  deriving (Eq, Ord, Show)

modifyRoll :: SkillRoll -> Int -> SkillRoll
modifyRoll (SkillRoll k m) m' = SkillRoll k (m + m')

modifiedRoll :: SkillRoll -> Int
modifiedRoll (SkillRoll 1 _) = 1           -- natural 1's always fail
modifiedRoll (SkillRoll k m) = max 1 (k+m) -- rolls cannot be modified below 1



-- basic roll: takes into account rerolls, modifiers and natural fails
-- * noModPass: required natural roll
-- * modPass: required modified roll
-- * both noModPass and modPass must be satisfied
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
        _                                -> False


-- skill roll: similar to `roll` but instead of fail/pass returns both modified
-- and unmodiified result
-- * instead of predicates requires minimum (unmodified/modified) values
-- * 1's always fail
skillRoll :: Reroll -> Int -> Int -> Int -> Prob SkillRoll
skillRoll rr noModSkill skill mods = do
    k <- d6

    if k == 1 then
      if rr /= NoReroll then
        skillRoll NoReroll noModSkill skill mods
      else
        return (SkillRoll 1 mods)
    else
      if k >= noModSkill && k + mods >= skill then
        return (SkillRoll k mods)
      else if rerollable k then
        skillRoll NoReroll noModSkill skill mods
      else
        return (SkillRoll k mods)
  where
    rerollable k =
      case rr of
        RerollOnes   | k == 1 -> True
        RerollFailed
          | k < noModSkill    -> True
          | k < skill         -> True
        RerollAll             -> True
        _                     -> False


data ChargeRerolls = NoChargeReroll | RerollChargeOneDie | RerollChargeAllDice | RerollChargeAnyDice


-- charge roll: returns sucess/fail
chargeRoll :: ChargeRerolls -> Int -> Prob Bool
chargeRoll rr minRoll = do
  (a, b) <- sorted2d6
  let dist = a + b

  if dist >= minRoll then
    return True
  else
    case rr of
      NoChargeReroll      -> return False
      RerollChargeOneDie  -> bernoulli (prob_d6_geq (minRoll-b))
      RerollChargeAllDice -> bernoulli baseProbPass
      RerollChargeAnyDice
        | baseProbPass >= prob_d6_geq (minRoll-b) -> bernoulli baseProbPass
        | otherwise                               -> bernoulli (prob_d6_geq (minRoll-b))
  where
    baseProbPass = probTrue $ chargeRoll NoChargeReroll minRoll


-- Data type for complex modifiers, including x2, ÷2 or constant values
-- Unlike functions, can be compared (even if not meaningfully) and shown.

data IntMod
    = NoMod                -- identity
    | ConstVal !Int        -- constant value (e.g. close combat weapons with fixed strength)
    | Add !Int             -- +k
    | Times !Int           -- ×2
    | Half                 -- ÷2, rounding up
    | Dot !IntMod !IntMod  -- composition
  deriving (Eq, Ord, Show)


-- Monoid homomorphism: IntMod -> Endo Int
applyIntMod :: IntMod -> Int -> Int
applyIntMod NoMod        n = n
applyIntMod (ConstVal k) _ = k
applyIntMod (Add k)      n = n + k
applyIntMod (Times k)    n = n * k
applyIntMod Half         n = (n+1) `div` 2
applyIntMod (Dot m1 m2)  n = applyIntMod m1 (applyIntMod m2 n)


-- Monoid instance: composition. Associativity is satisfied only when applied.
instance Semigroup IntMod where
    NoMod <> m     = m
    m     <> NoMod = m
    m1    <> m2    = Dot m1 m2

instance Monoid IntMod where
    mempty = NoMod


data Reroll
    = NoReroll     -- no re-rolls allowed
    | RerollOnes   -- re-roll ones
    | RerollFailed -- re-roll fails (before modifiers)
    | RerollAll    -- re-roll any (in practice, after modifiers)
  deriving (Eq, Ord, Show)


-- Monoid instance: take the best strategy for success (max)
instance Semigroup Reroll where
    (<>) = max
instance Monoid Reroll where
    mempty = NoReroll


-- Aggregation of possible roll bonuses: hit/wound/save/damage re-rolls, modifiers etc.
data RollMods = RollMods
  { _mod_tohit       :: !Int
  , _mod_rrtohit     :: !Reroll
  , _mod_towound     :: !Int
  , _mod_rrtowound   :: !Reroll
  , _mod_toarmor     :: !Int
  , _mod_rrarmor     :: !Reroll
  , _mod_toinv       :: !Int
  , _mod_rrinv       :: !Reroll
  , _mod_tobehit     :: !Int
  , _mod_tobewounded :: !Int
  , _mod_recvdmg     :: !IntMod
  }
  deriving (Eq, Ord, Show)

makeLenses ''RollMods

noMods :: RollMods
noMods = RollMods 0 NoReroll 0 NoReroll 0 NoReroll 0 NoReroll 0 0 NoMod


-- Sets both armor and invulnerable modifiers
mod_tosave :: Setter' RollMods Int
mod_tosave f mods =
    (\toarmor toinv -> mods { _mod_toarmor = toarmor, _mod_toinv = toinv })
      <$> f (_mod_toarmor mods) <*> f (_mod_toinv mods)


-- Sets both armor and invulnerable re-rolls
mod_rrtosave :: Setter' RollMods Reroll
mod_rrtosave f mods =
    (\rrarmor rrinv -> mods { _mod_rrarmor = rrarmor, _mod_rrinv = rrinv })
      <$> f (_mod_rrarmor mods) <*> f (_mod_rrinv mods)


-- Monoid instance: product of instances: add modifiers and max re-rolls
instance Semigroup RollMods where
    m1 <> m2 = RollMods
      { _mod_tohit       = _mod_tohit m1       +  _mod_tohit m2
      , _mod_rrtohit     = _mod_rrtohit m1     <> _mod_rrtohit m2
      , _mod_towound     = _mod_towound m1     +  _mod_towound m2
      , _mod_rrtowound   = _mod_rrtowound m1   <> _mod_rrtowound m2
      , _mod_toarmor     = _mod_toarmor m1     +  _mod_toarmor m2
      , _mod_rrarmor     = _mod_rrarmor m1     <> _mod_rrarmor m2
      , _mod_toinv       = _mod_toinv m1       +  _mod_toinv m2
      , _mod_rrinv       = _mod_rrinv m1       <> _mod_rrinv m2
      , _mod_tobehit     = _mod_tobehit m1     +  _mod_tobehit m2
      , _mod_tobewounded = _mod_tobewounded m1 +  _mod_tobewounded m2
      , _mod_recvdmg     = _mod_recvdmg m1     <> _mod_recvdmg m2
      }

instance Monoid RollMods where
    mempty = noMods


-- Auras: apply modifiers to other models

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
