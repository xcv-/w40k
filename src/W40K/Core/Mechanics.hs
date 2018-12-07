{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language RebindableSyntax #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
{-# language Strict #-}
module W40K.Core.Mechanics
  ( module Mechanics
  , sumWounds
  , sumWoundsMax
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

import Control.Lens

import Data.Function    (on)
import Data.List        (foldl', isInfixOf, sortBy)
import Data.Traversable (for)

import W40K.Core.SortedList (SortedList, SortedListItem(..))
import qualified W40K.Core.SortedList as SortedList

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Util ((:*:)(..))
import W40K.Core.Mechanics.Common as Mechanics
import W40K.Core.Mechanics.Melee  as Mechanics
import W40K.Core.Mechanics.Ranged as Mechanics


-- GENERAL CORE MECHANICS

type DamageRoll = Prob Int

newtype MortalWounds = MortalWounds { getMortalWounds :: DamageRoll }
  deriving (Eq, Ord, Show)

instance Semigroup MortalWounds where
    MortalWounds mw1 <> MortalWounds mw2 = MortalWounds (sumProbs [mw1, mw2])
instance Monoid MortalWounds where
    mempty = MortalWounds (return 0)


data Wounds w = Wounds {-# unpack #-} !Int !w
  deriving (Eq, Ord, Show)

instance Ord w => SortedListItem (Wounds w) where
    type ItemKey (Wounds w) = w

    itemKey (Wounds _ w) = w
    combineItems (Wounds n _) (Wounds n' w) = Wounds (n+n') w

data TotalWounds w = TotalWounds
  { _wounding_mortal :: !MortalWounds
  , _wounding_hits   :: !(SortedList (Wounds w))
  }
  deriving (Eq, Ord, Show)

makeLenses ''TotalWounds

instance Ord w => Semigroup (TotalWounds w) where
    TotalWounds w1 mw1 <> TotalWounds w2 mw2 = TotalWounds (w1 <> w2) (mw1 <> mw2)
instance Ord w => Monoid (TotalWounds w) where
    mempty = TotalWounds mempty mempty


satisfiesReqRoll :: RequiredRoll -> Int -> Int -> Bool
satisfiesReqRoll (MinModifiedRoll m)   _    r = r >= m
satisfiesReqRoll (MinUnmodifiedRoll m) mods r = r-mods >= m

rollHits :: IsWeapon w => Model -> w -> Model -> Prob Int
rollHits src w tgt = do
    natt <- weaponAttacks src w

    if w^.as_weapon.w_autohit then
      return natt
    else
      rollToHit src w tgt natt (w^.as_weapon.w_hooks.hook_hit)

rollToHit :: IsWeapon w => Model -> w -> Model -> Int -> [HitHook] -> Prob Int
rollToHit src w tgt natt hooks =
    case hooks of
      [] -> binomial natt pHit

      [RollHook reqRoll eff] -> do
        nhit <- binomial natt pHit
        neff <- binomial nhit $ min 1 (probOf (satisfiesReqRoll reqRoll mods) (hitRoll src w tgt) / pHit)
        nextra <- resolveHitHook src w tgt neff eff []
        return (nhit + nextra)

      _ ->
        -- NOTE: evaluated once and then summed n times
        sumIID natt $ do
          k <- hitRoll src w tgt
          let base = if doesHit src w tgt k then 1 else 0
          extra <- sumProbs $ resolveHooks (\req -> satisfiesReqRoll req mods k) (resolveHitHook src w tgt 1) hooks k
          return (base + extra)
  where
    pHit = probHit src w tgt
    mods = hitMods src w tgt

rollWounds :: IsWeapon w => Model -> w -> Model -> Int -> Prob (TotalWounds Weapon)
rollWounds src w tgt nhit =
    rollToWound src w tgt nhit (w^.as_weapon.w_hooks.hook_wound)

rollWounds' :: IsWeapon w => Model -> w -> Model -> Int -> (Int, Prob (TotalWounds Weapon))
rollWounds' src w tgt nhit =
    rollToWound' src w tgt nhit (w^.as_weapon.w_hooks.hook_wound)

rollToWound :: IsWeapon w => Model -> w -> Model -> Int -> [WoundHook] -> Prob (TotalWounds Weapon)
rollToWound src w tgt nhit hooks =
    case rollToWound' src w tgt nhit hooks of
      (mult, pwounds) -> foldIID mult pwounds

rollToWound' :: IsWeapon w => Model -> w -> Model -> Int -> [WoundHook] -> (Int, Prob (TotalWounds Weapon))
rollToWound' src w tgt nhit hooks = do
    case hooks of
      [] -> withMult 1 $ do
        nwound <- binomial nhit pWound
        let regularWounds = TotalWounds mempty (SortedList.singleton (Wounds nwound (w^.as_weapon)))
        return regularWounds

      [RollHook reqRoll eff] -> withMult 1 $ do
        nwound <- binomial nhit pWound
        neff   <- binomial nwound $ min 1 (probOf (satisfiesReqRoll reqRoll mods) (woundRoll src w tgt) / pWound)
        extraW <- resolveWoundHook src w tgt neff eff []

        let regularWounds = TotalWounds mempty (SortedList.singleton (Wounds nwound (w^.as_weapon)))
        return (regularWounds <> extraW)

      _ -> withMult nhit $ do
          k <- woundRoll src w tgt
          extra <- foldProbs $ resolveHooks (\req -> satisfiesReqRoll req mods k) (resolveWoundHook src w tgt 1) hooks k

          if doesWound src w tgt k then
            let regularWound = TotalWounds mempty (SortedList.singleton (Wounds 1 (w^.as_weapon)))
            in  return (regularWound <> extra)
          else
            return extra
  where
    pWound = probWound src w tgt
    mods = woundMods src w

    withMult !n !p = (n, p)


resolveHooks :: Show a => (RequiredRoll -> Bool) -> (eff -> [RollHook eff] -> Prob a) -> [RollHook eff] -> Int -> [Prob a]
resolveHooks filterHook resolveEff = go []
  where
    go _         []                                      _ = []
    go prevHooks (hook@(RollHook reqRoll eff):nextHooks) rollValue
      | filterHook reqRoll = resolveEff eff (prevHooks ++ nextHooks) : go (hook:prevHooks) nextHooks rollValue
      | otherwise          = go (hook:prevHooks) nextHooks rollValue

resolveHitHook :: IsWeapon w => Model -> w -> Model -> Int -> HitHookEff -> [HitHook] -> Prob Int
resolveHitHook src w tgt nsuccesses eff otherHooks =
    case eff of
      HitHookExtraHits    nhits -> return (nsuccesses * nhits)
      HitHookExtraAttacks natts -> rollToHit src w tgt (nsuccesses * natts) otherHooks

resolveWoundHook :: IsWeapon w => Model -> w -> Model -> Int -> WoundHookEff -> [WoundHook] -> Prob (TotalWounds Weapon)
resolveWoundHook src w tgt nsuccesses eff otherHooks =
    case eff of
      WoundHookExtraAttacks natt -> do
        nhit <- rollToHit src w tgt (natt * nsuccesses) (w^.as_weapon.w_hooks.hook_hit)
        rollToWound src w tgt nhit otherHooks
      WoundHookExtraHits nhit ->
        rollToWound src w tgt (nhit * nsuccesses) otherHooks
      WoundHookExtraWounds nwound ->
        return $ TotalWounds mempty (SortedList.singleton (Wounds (nwound * nsuccesses) (w^.as_weapon)))
      WoundHookMortalWounds pmw ->
        return $ TotalWounds (MortalWounds (sumIID nsuccesses pmw)) mempty
      WoundHookMortalDamage pmw ->
        return $ TotalWounds (MortalWounds (sumIID nsuccesses pmw))
                             (SortedList.singleton (Wounds (-nsuccesses) (w^.as_weapon)))
      WoundHookModWeapon w' -> do
        return $ TotalWounds mempty (SortedList.singleton (Wounds nsuccesses w'))
  where
    pWound = probWound src w tgt

unsavedDamage :: IsWeapon w => Model -> w -> Model -> Prob (TotalWounds DamageRoll)
unsavedDamage src w tgt = do
    nhit <- rollHits src w tgt

    let (mult, woundp) = rollWounds' src w tgt nhit

    let unsavedp = do
          TotalWounds mw whits <- woundp

          unsavedDmg <-
            forM (SortedList.toAscList whits) $ \(Wounds nwound w') -> do
              nunsaved <- binomial nwound $ probFailSave (w & as_weapon .~ w') tgt
              return (Wounds nunsaved (w'^.w_dmg))

          let unsavedDmg' = SortedList.fromAscList [wnd | wnd@(Wounds n _) <- unsavedDmg, n > 0]

          return (TotalWounds mw unsavedDmg')

    foldIID mult unsavedp

shrinkDamage :: SortedList (Wounds DamageRoll) -> SortedList (Wounds DamageRoll)
shrinkDamage = id

applyQuantumShielding :: Int -> DamageRoll
applyQuantumShielding 0   = return 0
applyQuantumShielding 1   = return 1
applyQuantumShielding dmg = do
    ignored <- roll NoReroll d6 (< dmg) (< dmg)
    if ignored then
        return 0
    else
        return dmg

applyFnp :: Int -> Int -> DamageRoll
applyFnp = \fnp nw ->
    if fnp >= 7 || nw == 0 then
        return nw
    else
        fmap (nw -) (ignoredWounds fnp nw)
  where
    ignoredWounds :: Int -> Int -> DamageRoll
    ignoredWounds fnp nw = cachedIgnoredWounds !! fnp !! nw

    cachedIgnoredWounds :: [[DamageRoll]]
    cachedIgnoredWounds = [[binomial nw (prob_d6_gt fnp) | nw <- [0..]] | fnp <- [0..7]]

effectiveDamage :: CombatType -> Model -> TotalWounds DamageRoll -> TotalWounds DamageRoll
effectiveDamage ct tgt unsaved =
    let (TotalWounds (MortalWounds mw) wounds) = unsaved

        mw' :: DamageRoll
        mw' = dmgAfterFnp mw

        modifyDamageRoll :: DamageRoll -> DamageRoll
        modifyDamageRoll = dmgAfterFnp . dmgAfterDmgMods . dmgAfterQuantumShielding

        wounds' :: SortedList (Wounds DamageRoll)
        wounds' = SortedList.map (\(Wounds nwounded pdmg) -> Wounds nwounded (modifyDamageRoll pdmg))
                                 wounds
    in
        TotalWounds (MortalWounds mw') wounds'
  where
    dmgAfterQuantumShielding :: DamageRoll -> DamageRoll
    dmgAfterQuantumShielding pdmg
      | tgt^.model_quantumShielding = applyQuantumShielding =<< pdmg
      | otherwise                   = pdmg

    dmgAfterDmgMods :: DamageRoll -> DamageRoll
    dmgAfterDmgMods pdmg =
        case tgt^.model_mods_for ct.mod_recvdmg of
          NoMod -> pdmg
          m     -> fmap (\dmg -> if dmg <= 0 then dmg else max 1 (applyIntMod m dmg))
                        pdmg

    dmgAfterFnp :: DamageRoll -> DamageRoll
    dmgAfterFnp pdmg = applyFnp (tgt^.model_fnp) =<< pdmg


evaluateAttack :: IsWeapon w => CombatType -> Model -> w -> Model -> Prob (TotalWounds DamageRoll)
evaluateAttack ct src w tgt = fmap (effectiveDamage ct tgt) (unsavedDamage src w tgt)

evaluateAttacks :: IsWeapon w => CombatType -> [(Model, w)] -> Model -> [Prob (TotalWounds DamageRoll)]
evaluateAttacks ct srcws tgt = map (\(src, w) -> evaluateAttack ct src w tgt) (shrinkModels srcws)


foldWounds :: forall a. Ord a => (a -> Int -> a) -> a -> [Prob (TotalWounds DamageRoll)] -> Prob a
foldWounds f = foldlM' (\a wnd -> consumeRound a |=<<| wnd)
  where
    consumeRound :: a -> TotalWounds DamageRoll -> Prob a
    consumeRound z (TotalWounds (MortalWounds pmw) weaponDmg) = do
      let mz' = pmw >>= \mw -> consumeWounds (return z) (Wounds mw (return 1))

      foldl' consumeWounds mz' (SortedList.toAscList weaponDmg)

    consumeWounds :: Prob a -> Wounds DamageRoll -> Prob a
    consumeWounds mz (Wounds n dmg) = foldlProbs' f mz (replicate n dmg)


sumWoundsMax :: Int -> [Prob (TotalWounds DamageRoll)] -> Prob Int
sumWoundsMax maxWounds = top . sumProbs . map (top . sumRound =<<)
  where
    top :: DamageRoll -> DamageRoll
    top | maxWounds > 0 = fmapProbMonotone (min maxWounds)
        | otherwise     = id

    sumTop :: Int -> Int -> Int
    sumTop | maxWounds > 0 = \x y -> min maxWounds (x+y)
           | otherwise     = (+)

    sumRound :: TotalWounds DamageRoll -> Prob Int
    sumRound (TotalWounds (MortalWounds pmw) weaponDmg) =
        sumProbs [pmw, sumWeaponDmg weaponDmg]

    sumWeaponDmg :: SortedList (Wounds DamageRoll) -> DamageRoll
    sumWeaponDmg =
        sumProbs . map (\(Wounds n pdmg) -> foldAssocIID sumTop 0 n pdmg) . SortedList.toAscList

sumWounds :: [Prob (TotalWounds DamageRoll)] -> Prob Int
sumWounds = sumWoundsMax 0

slainModels :: Model -> [Prob (TotalWounds DamageRoll)] -> Prob QQ
slainModels tgt =
    fmap summarize . foldWounds woundModels (0 :*: 0)
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
  deriving (Eq, Ord, Show)

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

evaluateCombat :: CombatType -> [EquippedModel] -> Model -> [Prob (TotalWounds DamageRoll)]
evaluateCombat ct srcs tgt =
    case ct of
      Melee  -> evaluateAttacks ct [(src^.em_model, src^.em_ccw) | src <- srcs                  ] tgt
      Ranged -> evaluateAttacks ct [(src^.em_model, rw)          | src <- srcs, rw <- src^.em_rw] tgt


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
numWounds ct srcs tgt = sumWounds $ evaluateCombat ct srcs tgt

numWoundsMax :: CombatType -> [EquippedModel] -> Model -> Int -> Prob Int
numWoundsMax ct srcs tgt maxWounds = sumWoundsMax maxWounds $ evaluateCombat ct srcs tgt

numSlainModels :: CombatType -> [EquippedModel] -> Model -> Prob QQ
numSlainModels ct srcs tgt = slainModels tgt $ evaluateCombat ct srcs tgt

numSlainModelsInt :: CombatType -> [EquippedModel] -> Model -> Prob Int
numSlainModelsInt ct srcs tgt = fmapProbMonotone floor $ numSlainModels ct srcs tgt

probKill :: CombatType -> [EquippedModel] -> Int -> Model -> QQ
probKill ct srcs 1     tgt = probOf (>= tgt^.model_wnd) (numWoundsMax ct srcs tgt (tgt^.model_wnd))
probKill ct srcs ntgts tgt = probOf (>= ntgts)          (numSlainModelsInt ct srcs tgt)
