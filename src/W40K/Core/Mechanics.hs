{-# language BangPatterns #-}
{-# language RankNTypes #-}
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
  , attackSplit
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
import Data.List        (foldl', isInfixOf, sortBy)
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

data TotalUnsavedWounds = TotalUnsavedWounds
  { _total_wounding_hits   :: ![(Weapon, Int)]
  , _total_wounding_mortal :: !MortalWounds
  }
  deriving (Eq, Ord)

makeLenses ''TotalUnsavedWounds

instance Monoid TotalUnsavedWounds where
    mempty = TotalUnsavedWounds mempty mempty
    mappend (TotalUnsavedWounds w1 mw1) (TotalUnsavedWounds w2 mw2) = TotalUnsavedWounds (mappend w1 w2) (mappend mw1 mw2)


unsavedWounds :: IsWeapon w => Model -> w -> Model -> Prob TotalUnsavedWounds
unsavedWounds src w tgt = do
    natt <- weaponAttacks src w
    totalHits <- rollToHit natt (w^.as_weapon.w_hooks.hook_hit)

    nwound    <- binomial totalHits pWound
    woundEffs <- getWoundEffs nwound [] (w^.as_weapon.w_hooks.hook_wound)

    let totalMW = mconcat [mw | (mw, _, _, _) <- woundEffs]

    let totalWound = nwound + sum [nextra | (_, nextra, _, _) <- woundEffs]

    nunsaved <- binomial (totalWound - sum [neff | (_, _, neff, _) <- woundEffs]) $ probFailSave w tgt

    let unsaved = (w^.as_weapon, nunsaved)

    unsavedEffs <-
      forM woundEffs $ \(_, _, neff, effw) -> do
        nunsavedEff <- binomial neff $ probFailSave (w & as_weapon .~ effw) tgt
        return (effw, nunsavedEff)

    let unsavedHits = [hit | hit@(_, n) <- unsaved:unsavedEffs, n > 0]

    return (TotalUnsavedWounds unsavedHits totalMW)
  where
    pHit :: QQ
    pHit = probHit src w tgt

    rollToHit :: Int -> [HitHook] -> Prob Int
    rollToHit natt hooks = do
      nhits     <- binomial natt pHit
      nextraHit <- sumProbs $ getAllExtraHits nhits [] hooks
      return (nhits + nextraHit)

    getAllExtraHits :: Int -> [HitHook] -> [HitHook] -> [Prob Int]
    getAllExtraHits nhits _         []           = []
    getAllExtraHits nhits prevHooks (hook:hooks) =
      let phit = getExtraHits nhits (prevHooks ++ hooks) hook
          phits = getAllExtraHits nhits (hook:prevHooks) hooks
      in  phit : phits

    getExtraHits :: Int -> [HitHook] -> HitHook -> Prob Int
    getExtraHits nhits otherHooks hook = do
      -- FIXME: not exactly correct (should be ok on average) if there is more
      -- than one hook: it produces impossible events, such as one hook firing
      -- on a 6+ and another not firing even though both depend on the same die
      -- roll
      -- TODO: should be fixed by case analysis on previous eff rolls + total probabilities
      -- but it could become too slow
      let minRoll = hook^.hook_minRoll - hitMods src w tgt
      nsuccesses <- binomial nhits $ probOf (>= minRoll) $ given (doesHit src w tgt) d6

      case hook^.hook_eff of
          HitHookExtraHits    extraHits -> return (extraHits * nsuccesses)
          HitHookExtraAttacks extraAtts -> rollToHit extraAtts otherHooks

    pWound :: QQ
    pWound = probWound src w tgt

    getWoundEffs :: Int -> [WoundHook] -> [WoundHook] -> Prob [(MortalWounds, Int, Int, Weapon)]
    getWoundEffs nwound _         []           = return []
    getWoundEffs nwound prevHooks (hook:hooks) = do
      woundEff  <- getWoundEff  nwound (prevHooks ++ hooks) hook
      woundEffs <- getWoundEffs nwound (hook:prevHooks) hooks
      return (woundEff ++ woundEffs)

    getWoundEff :: Int -> [WoundHook] -> WoundHook -> Prob [(MortalWounds, Int, Int, Weapon)]
    getWoundEff nwound otherHooks hook = do
      -- FIXME: not exactly correct (should be ok on average) if there is more
      -- than one hook: it produces impossible events, such as one hook firing
      -- on a 6+ and another not firing even though both depend on the same die
      -- roll
      -- TODO: should be fixed by case analysis on previous eff rolls + total probabilities,
      -- but it could become too slow
      let minRoll = hook^.hook_minRoll - woundMods src w
      nsuccesses <- binomial nwound $ probOf (>= minRoll) $ given (doesWound src w tgt) d6

      case hook^.hook_eff of
          WoundHookExtraAttacks natt -> do
            nhit <- rollToHit (natt * nsuccesses) (w^.as_weapon.w_hooks.hook_hit)
            nwound' <- binomial nhit pWound
            woundEffs <- getWoundEffs nwound' [] otherHooks
            return $ (MortalWounds 0, nwound', 0, w^.as_weapon) : woundEffs
          WoundHookExtraHits nhit -> do
            nwound' <- binomial (nhit * nsuccesses) pWound
            woundEffs <- getWoundEffs nwound' [] otherHooks
            return $ (MortalWounds 0, nwound', 0, w^.as_weapon) : woundEffs
          WoundHookExtraWounds nwound' -> do
            return [(MortalWounds 0, nwound' * nsuccesses, 0, w^.as_weapon)]
          WoundHookMortalWounds pmw -> do
            mw <- sumProbs (replicate nsuccesses pmw)
            return [(MortalWounds mw, 0, 0, w^.as_weapon)]
          WoundHookMortalDamage pmw -> do
            mw <- sumProbs (replicate nsuccesses pmw)
            return [(MortalWounds mw, -nsuccesses, 0, w^.as_weapon)]
          WoundHookModWeapon w' -> do
            return [(MortalWounds 0, 0, nsuccesses, w')]


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
        unsavedResult (src, w) = unsavedWounds src w tgt

        totalUnsaved :: Prob TotalUnsavedWounds
        totalUnsaved = foldProbs (map unsavedResult (shrinkModels srcws))
    in
        fmap (total_wounding_hits %~ shrinkWounds) totalUnsaved


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

    mw' <- woundsAfterFnp mw
    acc <- foldlProbs' f start dmgSeq
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

attackSplit :: Int -> CCWeapon -> EquippedModel -> [EquippedModel]
attackSplit n ccw em =
    [ em & em_model.model_att -~ n
    , em & em_model.model_att .~ n
         & em_ccw .~ ccw
    ]


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
