{-# language BangPatterns #-}
{-# language RebindableSyntax #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module W40K.Core.Mechanics
  ( module Mechanics
  , sumWounds
  , slainModels
  , EquippedModel (..)
  , em_model, em_ccw, em_rw, em_name
  , basicEquippedModel
  , Modifier
  , with
  , applyAura
  , within
  , twoHighest
  , numWounds
  , numWoundsMax
  , numSlainModels
  , numSlainModelsInt
  , probKill
  ) where

import Prelude hiding (Functor(..), Monad(..), (=<<))
import Data.Function    (on)
import Data.List        (foldl', sortBy)
import Data.Traversable (for)
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics.Common as Mechanics
import W40K.Core.Mechanics.Melee  as Mechanics
import W40K.Core.Mechanics.Ranged as Mechanics

-- GENERAL CORE MECHANICS

newtype MortalWounds = MortalWounds { getMortalWounds :: Int }
  deriving (Eq, Ord)

instance Monoid MortalWounds where
    mempty = MortalWounds 0
    mappend (MortalWounds mw1) (MortalWounds mw2) = MortalWounds (mw1+mw2)

data UnsavedWounds = UnsavedWounds
  { _unsaved_mortal    :: !MortalWounds
  , _unsaved_wnd       :: !Int
  , _unsaved_wndEff    :: !Int
  , _unsaved_effWeapon :: !Weapon
  }
  deriving (Eq, Ord)

makeLenses ''UnsavedWounds

data TotalUnsavedWounds = TotalUnsavedWounds
  { _total_wounding_weapons :: ![(Weapon, Int)]
  , _total_wounding_mortal :: !MortalWounds
  }
  deriving (Eq, Ord)

makeLenses ''TotalUnsavedWounds

instance Monoid TotalUnsavedWounds where
    mempty = TotalUnsavedWounds mempty mempty
    mappend (TotalUnsavedWounds w1 mw1) (TotalUnsavedWounds w2 mw2) = TotalUnsavedWounds (mappend w1 w2) (mappend mw1 mw2)


unsavedWounds :: IsWeapon w => Model -> w -> Model -> Prob UnsavedWounds
unsavedWounds src w tgt = do
    natt <- weaponAttacks src w
    totalHits <- rollToHit natt

    nwound                   <- binomial totalHits pWound
    (mw, nextra, neff, effw) <- getWoundEff nwound (w^.as_weapon.w_hooks.hook_wound)
    let totalWound = nwound + nextra

    nunsavedEff <- binomial neff                $ probFailSave (w & as_weapon .~ effw) tgt
    nunsaved    <- binomial (totalWound - neff) $ probFailSave w                       tgt
    return (UnsavedWounds mw nunsaved nunsavedEff effw)
  where
    pHit :: QQ
    pHit = probHit src w tgt

    rollToHit :: Int -> Prob Int
    rollToHit natt = do
      nhits     <- binomial natt pHit
      nextraHit <- getExtraHits nhits (w^.as_weapon.w_hooks.hook_hit)
      return (nhits + nextraHit)

    getExtraHits :: Int -> Maybe HitHook -> Prob Int
    getExtraHits _     Nothing     = return 0
    getExtraHits nhits (Just hook) = do
      let minRoll = hook^.hook_minRoll - hitMods src w tgt
      nsuccesses <- binomial nhits $ probOf (>= minRoll) $ given (doesHit src w tgt) d6

      case hook^.hook_eff of
          HitHookExtraHits    extraHits -> return (extraHits * nsuccesses)
          HitHookExtraAttacks extraAtts -> binomial (extraAtts * nsuccesses) pHit

    pWound :: QQ
    pWound = probWound src w tgt

    getWoundEff :: Int -> Maybe WoundHook -> Prob (MortalWounds, Int, Int, Weapon)
    getWoundEff _      Nothing     = return (MortalWounds 0, 0, 0, w^.as_weapon)
    getWoundEff nwound (Just hook) = do
      let minRoll = hook^.hook_minRoll - woundMods src w
      nsuccesses <- binomial nwound $ probOf (>= minRoll) $ given (doesWound src w tgt) d6

      case hook^.hook_eff of
          WoundHookExtraAttacks natt -> do
            nhit <- rollToHit (natt * nsuccesses)
            nwound' <- binomial nhit pWound
            return (MortalWounds 0, nwound', 0, w^.as_weapon)
          WoundHookExtraHits nhit -> do
            nwound' <- binomial (nhit * nsuccesses) pWound
            return (MortalWounds 0, nwound', 0, w^.as_weapon)
          WoundHookExtraWounds nwound' -> do
            return (MortalWounds 0, nwound' * nsuccesses, 0, w^.as_weapon)
          WoundHookMortalWounds pmw -> do
            mw <- sumProbs (replicate nsuccesses pmw)
            return (MortalWounds mw, 0, 0, w^.as_weapon)
          WoundHookModWeapon w' -> do
            return (MortalWounds 0, 0, nsuccesses, w')


shrinkWounds :: [(Weapon, Int)] -> [(Weapon, Int)]
shrinkWounds = groupWith (equivDmg `on` fst) totalWounding . sortBy (compare `on` fst)
  where
    equivDmg :: Weapon -> Weapon -> Bool
    equivDmg = (==) -- TODO weaken

    totalWounding :: (Weapon, Int) -> [(Weapon, Int)] -> (Weapon, Int)
    totalWounding (w, nw) wnws =
        let !nw' = nw + sum (map snd wnws)
        in  (w, nw')


woundingResult :: forall w. IsWeapon w => [(Model, w)] -> Model -> Prob TotalUnsavedWounds
woundingResult srcws tgt =
    let unsavedResult :: (Model, w) -> Prob TotalUnsavedWounds
        unsavedResult (src, w) = do
            unsaved <- unsavedWounds src w tgt
            let normal = (w^.as_weapon,               unsaved^.unsaved_wnd)
                eff    = (unsaved^.unsaved_effWeapon, unsaved^.unsaved_wndEff)
                mortal = unsaved^.unsaved_mortal
            return TotalUnsavedWounds
                { _total_wounding_weapons = filter (\(_, n) -> n /= 0) [normal, eff]
                , _total_wounding_mortal  = mortal
                }

        totalUnsaved :: Prob TotalUnsavedWounds
        totalUnsaved = foldProbs (map unsavedResult (shrinkModels srcws))
    in
        fmap (total_wounding_weapons %~ shrinkWounds) totalUnsaved


applyQuantumShielding :: Int -> Prob Int
applyQuantumShielding 0   = return 0
applyQuantumShielding 1   = return 1
applyQuantumShielding dmg = do
    ignored <- roll NoReroll d6 (< dmg) (< dmg)
    if ignored then
        return 0
    else
        return dmg

applyDamageModifier :: CombatType -> Model -> Int -> Int
applyDamageModifier ct tgt 0   = 0
applyDamageModifier ct tgt dmg = max 1 $ applyIntMod (tgt^.model_mods_for ct.mod_recvdmg) dmg

foldWounds :: forall a. (Ord a, Show a)
           => CombatType
           -> Model
           -> (a -> Int -> a)
           -> a
           -> TotalUnsavedWounds
           -> Prob a
foldWounds ct tgt f start totalUnsaved = do
    let (TotalUnsavedWounds wounds (MortalWounds mw)) = totalUnsaved

        dmgSeq :: [Prob Int]
        dmgSeq = concatMap (\(w, nwounded) -> replicate nwounded (w^.w_dmg)) wounds
               & map (dmgAfterFnp . dmgAfterDmgMods . dmgAfterQuantumShielding)

    acc <- foldlProbs' f start dmgSeq
    mw' <- woundsAfterFnp mw
    let acc' = foldl' f acc (replicate mw' 1)

    return acc'
  where
    dmgAfterQuantumShielding :: Prob Int -> Prob Int
    dmgAfterQuantumShielding pdmg
      | tgt^.model_quantumShielding = applyQuantumShielding =<< pdmg
      | otherwise                   = pdmg

    dmgAfterDmgMods :: Prob Int -> Prob Int
    dmgAfterDmgMods pdmg
      | tgt^.model_mods_for ct.mod_recvdmg == NoMod = pdmg
      | otherwise                                   = fmap (applyDamageModifier ct tgt) pdmg

    dmgAfterFnp :: Prob Int -> Prob Int
    dmgAfterFnp pdmg = woundsAfterFnp =<< pdmg

    woundsAfterFnp :: Int -> Prob Int
    woundsAfterFnp 0       = return 0
    woundsAfterFnp wounds
      | tgt^.model_fnp >= 7 = return wounds
      | otherwise           = do
          ignoredWounds <- cachedFnpIgnoredWounds !! wounds
          return (wounds - ignoredWounds)

    cachedFnpIgnoredWounds :: [Prob Int]
    cachedFnpIgnoredWounds = [binomial dmg (prob_d6_gt (tgt^.model_fnp)) | dmg <- [0..]]

sumWounds :: CombatType -> Model -> TotalUnsavedWounds -> Prob Int
sumWounds ct tgt = foldWounds ct tgt (+) 0

sumWoundsMax :: CombatType -> Model -> Int -> TotalUnsavedWounds -> Prob Int
sumWoundsMax ct tgt maxWounds = foldWounds ct tgt f 0
  where
    f acc wnd
      | acc + wnd >= maxWounds = maxWounds
      | otherwise              = acc + wnd

slainModels :: CombatType -> Model -> TotalUnsavedWounds -> Prob QQ
slainModels ct tgt =
    fmap summarize . foldWounds ct tgt woundModels (0 :*: 0)
  where
    woundModels :: (Int :*: Int) -> Int -> (Int :*: Int)
    woundModels (kills :*: wounds) dmg
      | wounds + dmg >= tgt^.model_wnd = (kills+1 :*: 0)
      | otherwise                      = (kills :*: wounds + dmg)

    summarize :: (Int :*: Int) -> QQ
    summarize (kills :*: wounds) =
        fromIntegral kills + fromIntegral wounds / fromIntegral (tgt^.model_wnd)



-- EQUIPPED MODELS & UTILS

data EquippedModel = EquippedModel
  { _em_model    :: Model
  , _em_ccw      :: CCWeapon
  , _em_rw       :: [RngWeapon]
  }

makeLenses ''EquippedModel

instance IsModel EquippedModel where
    as_model = em_model

em_name :: Lens' EquippedModel String
em_name = em_model.model_name

basicEquippedModel :: Model -> EquippedModel
basicEquippedModel model = EquippedModel model basic_ccw []


type Modifier = EquippedModel -> EquippedModel

with :: Modifier -> [EquippedModel] -> [EquippedModel]
with = map

applyAura :: Aura -> Modifier
applyAura aura = (em_model.model_cc_mods  <>~ aura^.aura_cc)
               . (em_model.model_rng_mods <>~ aura^.aura_rng)

within :: Aura -> [EquippedModel] -> [EquippedModel]
within = map . applyAura

twoHighest :: Ord a => a -> a -> a -> (a, a)
twoHighest a b c
  | a <= b && a <= c = (b, c)
  | b <= a && b <= c = (a, c)
  | otherwise        = (a, b)


-- ANALYSES

numWounds :: CombatType -> [EquippedModel] -> Model -> Prob Int
numWounds ct srcs tgt =
    case ct of
      Melee  -> sumWounds ct tgt |=<<| woundingResult [(src^.em_model, src^.em_ccw) | src <- srcs                  ] tgt
      Ranged -> sumWounds ct tgt |=<<| woundingResult [(src^.em_model, rw)          | src <- srcs, rw <- src^.em_rw] tgt

numWoundsMax :: CombatType -> [EquippedModel] -> Model -> Int -> Prob Int
numWoundsMax ct srcs tgt maxWounds =
    case ct of
      Melee  -> sumWoundsMax ct tgt maxWounds |=<<| woundingResult [(src^.em_model, src^.em_ccw) | src <- srcs                  ] tgt
      Ranged -> sumWoundsMax ct tgt maxWounds |=<<| woundingResult [(src^.em_model, rw)          | src <- srcs, rw <- src^.em_rw] tgt

numSlainModels :: CombatType -> [EquippedModel] -> Model -> Prob QQ
numSlainModels ct srcs tgt =
    case ct of
      Melee  -> slainModels ct tgt |=<<| woundingResult [(src^.em_model, src^.em_ccw) | src <- srcs                  ] tgt
      Ranged -> slainModels ct tgt |=<<| woundingResult [(src^.em_model, rw)          | src <- srcs, rw <- src^.em_rw] tgt

numSlainModelsInt :: CombatType -> [EquippedModel] -> Model -> Prob Int
numSlainModelsInt ct srcs tgt = fmap floor $ numSlainModels ct srcs tgt

probKill :: CombatType -> [EquippedModel] -> Int -> Model -> QQ
probKill ct srcs 1     tgt = probOf (>= tgt^.model_wnd) (numWoundsMax ct srcs tgt (tgt^.model_wnd))
probKill ct srcs ntgts tgt = probOf (>= ntgts)          (numSlainModelsInt ct srcs tgt)
