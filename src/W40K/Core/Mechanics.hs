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
  , numSlainModels
  , numSlainModelsInt
  , probKill
  ) where

import Prelude hiding (Functor(..), Monad(..), (=<<))
import Data.Function    (on)
import Data.List        (sortBy)
import Data.Traversable (for)
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics.Common as Mechanics
import W40K.Core.Mechanics.Melee  as Mechanics
import W40K.Core.Mechanics.Ranged as Mechanics

-- GENERAL CORE MECHANICS

data UnsavedWounds = UnsavedWounds
  { _unsaved_mw        :: !Int
  , _unsaved_wnd       :: !Int
  , _unsaved_wndEff    :: !Int
  , _unsaved_effWeapon :: !Weapon
  }
  deriving (Eq, Ord)

makeLenses ''UnsavedWounds


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

    getWoundEff :: Int -> Maybe WoundHook -> Prob (Int, Int, Int, Weapon)
    getWoundEff _      Nothing     = return (0, 0, 0, w^.as_weapon)
    getWoundEff nwound (Just hook) = do
      let minRoll = hook^.hook_minRoll - woundMods src w
      nsuccesses <- binomial nwound $ probOf (>= minRoll) $ given (doesWound src w tgt) d6

      case hook^.hook_eff of
          WoundHookExtraAttacks natt -> do
            nhit <- rollToHit (natt * nsuccesses)
            nwound' <- binomial nhit pWound
            return (0, nwound', 0, w^.as_weapon)
          WoundHookExtraHits nhit -> do
            nwound' <- binomial (nhit * nsuccesses) pWound
            return (0, nwound', 0, w^.as_weapon)
          WoundHookExtraWounds nwound' -> do
            return (0, nwound' * nsuccesses, 0, w^.as_weapon)
          WoundHookMortalWounds pmw -> do
            mw <- sumProbs (replicate nsuccesses pmw)
            return (mw, 0, 0, w^.as_weapon)
          WoundHookModWeapon w' -> do
            return (0, 0, nsuccesses, w')


shrinkWounds :: [(Weapon, Int)] -> [(Weapon, Int)]
shrinkWounds = groupWith (equivDmg `on` fst) sumDmg . sortBy (compare `on` fst)
  where
    equivDmg :: Weapon -> Weapon -> Bool
    equivDmg = (==) -- TODO weaken

    sumDmg (w, nw) wnws =
        let !nw' = nw + sum (map snd wnws)
        in  (w, nw')

shrinkDmg :: CombatType -> Model -> Int -> Prob Int -> Prob Int
shrinkDmg _  tgt 1          = \_ -> return 1
shrinkDmg ct tgt woundsLeft = fmap minimize
  where
    minimize :: Int -> Int
    minimize !dmg
      | max 1 (applyIntMod recvdmgmod (dmg-1)) < woundsLeft = dmg
      | otherwise                                           = minimize (dmg-1)

    recvdmgmod = tgt^.model_mods_for ct.mod_recvdmg

woundingResult :: forall w. IsWeapon w => [(Model, w)] -> Model -> Prob [(Weapon, Int)]
woundingResult srcws tgt =
    let unsavedResult :: (Model, w) -> Prob [(Weapon, Int)]
        unsavedResult (src, w) = do
                             unsaved <- unsavedWounds src w tgt
                             let mw     = unsaved^.unsaved_mw -- TODO count mortal wounds
                                 normal = (w^.as_weapon,               unsaved^.unsaved_wnd)
                                 eff    = (unsaved^.unsaved_effWeapon, unsaved^.unsaved_wndEff)
                             return (filter (\(_, n) -> n /= 0) [normal, eff])

        unsavedResults :: [Prob [(Weapon, Int)]]
        unsavedResults = map unsavedResult (shrinkModels srcws)

        together :: Prob [(Weapon, Int)]
        together = foldrProbs [] (++) unsavedResults
    in
        fmap shrinkWounds together


maxWounds :: [Prob Int] -> Int
maxWounds = sum . map maxEvent

foldWounds :: forall a. (Ord a, Show a)
           => CombatType
           -> Model
           -> (a -> Int)
           -> (a -> Int -> a)
           -> a
           -> [(Weapon, Int)]
           -> Prob a
foldWounds ct tgt woundsToKill f start = switch
  where
    quantumShielding :: Bool
    quantumShielding = tgt^.model_quantumShielding

    switch :: [(Weapon, Int)] -> Prob a
    switch wounds =
        let
          dmgSeq = concatMap (\(w, nwounded) -> replicate nwounded (w^.w_dmg)) wounds
        in
          if maxWounds dmgSeq < woundsToKill start && not quantumShielding then
            fmap (f start) (sumProbs dmgSeq)
          else
            consume start dmgSeq

    consume :: a -> [Prob Int] -> Prob a
    consume !acc []           = return acc
    consume !acc (pdmg:pdmgs)
      | quantumShielding    = do
          dmg <- pdmg
          if dmg <= 1 then
              continueWith pdmgs acc dmg
          else do
              ignored <- roll NoReroll d6 (< dmg) (< dmg)
              if ignored then
                  consume acc pdmgs
              else
                  continueWith pdmgs acc dmg
      | tgt^.model_fnp >= 7 = continueWith pdmgs acc =<< shrinkDmg ct tgt (woundsToKill acc) pdmg
      | otherwise           = continueWith pdmgs acc =<< pdmg

    continueWith :: [Prob Int] -> a -> Int -> Prob a
    continueWith pdmgs !acc !dmg = do
        let dmg' = max 1 $ applyIntMod (tgt^.model_mods_for ct.mod_recvdmg) dmg

        if tgt^.model_fnp < 7 then do
          savedWounds <- cachedFnpSavedWounds !! dmg'
          consume (f acc (dmg' - savedWounds)) pdmgs
        else
          consume (f acc dmg') pdmgs

    cachedFnpSavedWounds :: [Prob Int]
    cachedFnpSavedWounds = [binomial dmg (prob_d6_gt $ tgt^.model_fnp) | dmg <- [0..]]


sumWounds :: CombatType -> Model -> [(Weapon, Int)] -> Prob Int
sumWounds ct tgt wounds = do
    totalDmg <- foldWounds ct (tgt & model_fnp .~ 7) (const maxBound) (+) 0 wounds
    savedWounds <- binomial totalDmg (prob_d6_gt $ tgt^.model_fnp)
    return (totalDmg - savedWounds)

slainModels :: CombatType -> Model -> [(Weapon, Int)] -> Prob QQ
slainModels ct tgt =
    fmap summarize . foldWounds ct tgt (\(_ :*: wounds) -> tgt^.model_wnd - wounds) woundModels (0 :*: 0)
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
  , _em_rw       :: RngWeapon
  }

makeLenses ''EquippedModel

em_name :: Lens' EquippedModel String
em_name = em_model.model_name

basicEquippedModel :: Model -> EquippedModel
basicEquippedModel model = EquippedModel model basic_ccw null_rw


type Modifier = [EquippedModel] -> [EquippedModel]

with :: (EquippedModel -> EquippedModel) -> [EquippedModel] -> [EquippedModel]
with = map

applyAura :: Aura -> EquippedModel -> EquippedModel
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
      Melee  -> sumWounds ct tgt |=<<| woundingResult [(src^.em_model, src^.em_ccw) | src <- srcs] tgt
      Ranged -> sumWounds ct tgt |=<<| woundingResult [(src^.em_model, src^.em_rw)  | src <- srcs] tgt

numSlainModels :: CombatType -> [EquippedModel] -> Model -> Prob QQ
numSlainModels ct srcs tgt =
    case ct of
      Melee  -> slainModels ct tgt |=<<| woundingResult [(src^.em_model, src^.em_ccw) | src <- srcs] tgt
      Ranged -> slainModels ct tgt |=<<| woundingResult [(src^.em_model, src^.em_rw)  | src <- srcs] tgt

numSlainModelsInt :: CombatType -> [EquippedModel] -> Model -> Prob Int
numSlainModelsInt ct srcs tgt = fmap floor $ numSlainModels ct srcs tgt

probKill :: CombatType -> [EquippedModel] -> Int -> Model -> QQ
probKill ct srcs 1     tgt = probOf (>= tgt^.model_wnd) (numWounds ct srcs tgt)
probKill ct srcs ntgts tgt = probOf (>= ntgts)          (numSlainModelsInt ct srcs tgt)
