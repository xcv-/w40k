{-# language BangPatterns #-}
{-# language RankNTypes #-}
{-# language RebindableSyntax #-}
{-# language ScopedTypeVariables #-}
{-# language Strict #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
module W40K.Core.Mechanics.Attack
  ( MortalWounds(..)

  , TotalWounds(..)
  , DamageRoll
  , TotalDamage

  , TotalDamageWithHooks(..)

  , effectiveMortalWounds
  , effectiveDamage

  , sumWounds
  , sumWoundsMax

  , WoundedModels(..)
  , woundModels
  , fracWoundedModels

  , numWounds
  , numWoundsMax
  , resumeNumWoundedModels
  , numWoundedModels
  , numSlainModels
  , probKill
  ) where

import Prelude hiding (Functor(..), Monad(..), (=<<))

import Control.Lens

import Data.List (foldl')

import W40K.Core.SortedList (SortedList, SortedListItem(..))
import qualified W40K.Core.SortedList as SortedList

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Util (parMap)
import W40K.Core.Mechanics.Equipped
import W40K.Core.Mechanics.Model
import W40K.Core.Mechanics.Roll
import W40K.Core.Mechanics.Weapon


-- WOUND TYPES

-- Mortal wounds

newtype MortalWounds = MortalWounds { getMortalWounds :: Prob Int }
  deriving (Eq, Ord, Show)

instance Semigroup MortalWounds where
    MortalWounds mw1 <> MortalWounds mw2 = MortalWounds (sumProbs [mw1, mw2])

instance Monoid MortalWounds where
    mempty = MortalWounds (return 0)


-- Regular (weapon) wounds

data Wounds w = Wounds
    {-# unpack #-} Int -- multiplicity
                   w   -- weapon or damage roll
  deriving (Eq, Ord, Show)

instance Ord w => SortedListItem (Wounds w) where
    type ItemKey (Wounds w) = w

    itemKey (Wounds _ w) = w
    combineItems (Wounds n _) (Wounds n' w) = Wounds (n+n') w


-- Mortal wounds + regular (weapon) wounds

data TotalWounds w = TotalWounds MortalWounds (SortedList (Wounds w))
  deriving (Eq, Ord, Show)


instance Ord w => Semigroup (TotalWounds w) where
    TotalWounds w1 mw1 <> TotalWounds w2 mw2 = TotalWounds (w1 <> w2) (mw1 <> mw2)

instance Ord w => Monoid (TotalWounds w) where
    mempty = TotalWounds mempty mempty


type DamageRoll = Prob Int
type TotalDamage = TotalWounds DamageRoll

data TotalDamageWithHooks = TotalDamageWithHooks (Maybe DmgHook) TotalDamage
  deriving (Eq, Ord, Show)


---- HIT ROLLS

rollHits :: IsWeapon w => Model -> w -> Model -> Prob Int
rollHits src w tgt = do
    natt <- weaponAttacks src w

    if w^.as_weapon.w_autohit then
      return natt
    else
      rollToHit src w tgt natt (w^.as_weapon.w_hooks.hook_hit)


-- rollHits with fixed number of attacks and given hooks (internal)
rollToHit :: IsWeapon w => Model -> w -> Model -> Int -> [HitHook] -> Prob Int
rollToHit src w tgt natt hooks =
    case hooks of
      [] -> binomial natt pHit

      [RollHook reqRoll eff] -> do
        nhit <- binomial natt pHit
        neff <- binomial nhit $ min 1 (probOf (satisfiesReqRoll reqRoll) (hitRoll src w tgt) / pHit)
        nextra <- resolveHitHook src w tgt neff eff []
        return (nhit + nextra)

      _ ->
        -- NOTE: evaluated once and then summed n times
        sumIID natt $ do
          r <- hitRoll src w tgt
          let base = if doesHit src w tgt r then 1 else 0
          extra <- sumProbs $ resolveHooks (resolveHitHook src w tgt 1) r hooks
          return (base + extra)
  where
    pHit = probHit src w tgt


-- WOUND ROLLS

-- result: wounds (aggregated)
rollWounds :: IsWeapon w => Model -> w -> Model -> Int -> Prob (TotalWounds Weapon)
rollWounds src w tgt nhit =
    rollToWound src w tgt nhit (w^.as_weapon.w_hooks.hook_wound)


-- result: (multiplicity, wounds)
rollWounds' :: IsWeapon w => Model -> w -> Model -> Int -> (Int, Prob (TotalWounds Weapon))
rollWounds' src w tgt nhit =
    rollToWound' src w tgt nhit (w^.as_weapon.w_hooks.hook_wound)


-- rollWounds with given hooks (internal)
rollToWound :: IsWeapon w => Model -> w -> Model -> Int -> [WoundHook] -> Prob (TotalWounds Weapon)
rollToWound src w tgt nhit hooks =
    case rollToWound' src w tgt nhit hooks of
      (mult, pwounds) -> foldIID mult pwounds


-- rollWounds' with given hooks (internal)
rollToWound' :: IsWeapon w => Model -> w -> Model -> Int -> [WoundHook] -> (Int, Prob (TotalWounds Weapon))
rollToWound' src w tgt nhit hooks = do
    case hooks of
      [] -> withMult 1 $ do
        nwound <- binomial nhit pWound
        let regularWounds = TotalWounds mempty (SortedList.singleton (Wounds nwound (w^.as_weapon)))
        return regularWounds

      [RollHook reqRoll eff] -> withMult 1 $ do
        nwound <- binomial nhit pWound
        neff   <- binomial nwound $ min 1 (probOf (satisfiesReqRoll reqRoll) (woundRoll src w tgt) / pWound)
        extraW <- resolveWoundHook src w tgt neff eff []

        let regularWounds = TotalWounds mempty (SortedList.singleton (Wounds nwound (w^.as_weapon)))
        return (regularWounds <> extraW)

      _ -> withMult nhit $ do
          r <- woundRoll src w tgt
          extra <- foldProbs $ resolveHooks (resolveWoundHook src w tgt 1) r hooks

          if doesWound src w tgt r then
            let regularWound = TotalWounds mempty (SortedList.singleton (Wounds 1 (w^.as_weapon)))
            in  return (regularWound <> extra)
          else
            return extra
  where
    pWound = probWound src w tgt

    withMult n p = (n, p)


---- ROLLHOOK LOGIC

-- Generic hook resolution higher-order function
resolveHooks :: Show a => (eff -> [RollHook eff] -> Prob a) -> SkillRoll -> [RollHook eff] -> [Prob a]
resolveHooks resolveEff r = go r []
  where
    go _ _         []                                  = []
    go r prevHooks (hook@(RollHook req eff):nextHooks)
      | satisfiesReqRoll req r = resolveEff eff (prevHooks ++ nextHooks) : go r (hook:prevHooks) nextHooks
      | otherwise              = go r (hook:prevHooks) nextHooks


-- Argument for resolveHooks (eff ~ HitHookEff)
resolveHitHook :: IsWeapon w => Model -> w -> Model -> Int -> HitHookEff -> [HitHook] -> Prob Int
resolveHitHook src w tgt nsuccesses eff otherHooks =
    case eff of
      HitHookExtraHits    nhits -> return (nsuccesses * nhits)
      HitHookExtraAttacks natts -> rollToHit src w tgt (nsuccesses * natts) otherHooks


-- Argument for resolveHooks (eff ~ WoundHookEff)
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


-- Roll saving throws, simplifying weapon wounds into damage rolls
rollSaves :: IsWeapon w => TotalWounds Weapon -> w -> Model -> Prob TotalDamage
rollSaves (TotalWounds mw whits) w tgt = do
    unsavedDmg <-
      forM (SortedList.toAscList whits) $ \(Wounds nwound w') -> do
        nunsaved <- binomial nwound $ probFailSave (w & as_weapon .~ w') tgt
        return (Wounds nunsaved (w'^.w_dmg))

    let unsavedDmg' = SortedList.fromAscList [wnd | wnd@(Wounds n _) <- unsavedDmg, n > 0]

    return (TotalWounds mw unsavedDmg')


-- Higher level function: Roll hits, wounds and saving throws and zip with weapon damage hooks
unsavedDamage :: IsWeapon w => Model -> w -> Model -> Prob TotalDamageWithHooks
unsavedDamage src w tgt = do
    (mult, woundp) <- fmapProb (rollWounds' src w tgt) (rollHits src w tgt)

    dmg <- foldIID mult $ do
          wounds <- woundp
          rollSaves wounds w tgt

    return $ TotalDamageWithHooks (w^.as_weapon.w_hooks.hook_dmg) dmg


-- DAMAGE ROLLS

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
    cachedIgnoredWounds = [[binomial nw (prob_d6_geq fnp) | nw <- [0..]] | fnp <- [0..7]]


-- Utility function to apply FnP and other damage reductions to mortal wounds
-- and obtain a TotalWounds with only MW content.
effectiveMortalWounds :: Model -> MortalWounds -> TotalWounds a
effectiveMortalWounds tgt (MortalWounds mw) =
    TotalWounds (MortalWounds (dmgAfterFnp mw)) SortedList.empty
  where
    dmgAfterFnp :: DamageRoll -> DamageRoll
    dmgAfterFnp pdmg = applyFnp (tgt^.model_fnp) =<< pdmg


-- Main damage treatment function. Applies quantum shielding, damage reduction
-- and FnP to unsaved damage.
effectiveDamage :: CombatType -> Model -> TotalDamage -> TotalDamage
effectiveDamage ct tgt unsaved =
    let (TotalWounds mw wounds) = unsaved

        mw' :: MortalWounds
        mw' = MortalWounds (dmgAfterFnp (getMortalWounds mw))

        modifyDamageRoll :: DamageRoll -> DamageRoll
        modifyDamageRoll = dmgAfterFnp . dmgAfterDmgMods . dmgAfterQuantumShielding

        wounds' :: SortedList (Wounds DamageRoll)
        wounds' = SortedList.map (\(Wounds nwounded pdmg) -> Wounds nwounded (modifyDamageRoll pdmg))
                                 wounds
    in
        TotalWounds mw' wounds'
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


-- WHOLE ATTACK SEQUENCE

-- resolve attacks from a single model+weapon
resolveWeaponAttack :: IsWeapon w => CombatType -> Model -> w -> Model -> Prob TotalDamageWithHooks
resolveWeaponAttack ct src w tgt = do
    TotalDamageWithHooks hook wounds <- unsavedDamage src w tgt
    return (TotalDamageWithHooks hook (effectiveDamage ct tgt wounds))


-- resolve attacks from multiple model+weapons
resolveAttacks :: IsWeapon w => CombatType -> [(Model, w)] -> Model -> [Prob TotalDamageWithHooks]
resolveAttacks ct srcws tgt = concat $
    parMap (\(cnt, src, w) -> replicate cnt $ resolveWeaponAttack ct src w tgt)
           (shrinkModels srcws)


-- resolve attacks from multiple EquippedModels in a given phase
resolveCombat :: CombatType -> [EquippedModel] -> Model -> [Prob TotalDamageWithHooks]
resolveCombat ct srcs tgt =
    case ct of
      Melee  -> resolveAttacks ct [(src^.em_model, src^.em_ccw) | src <- srcs                  ] tgt
      Ranged -> resolveAttacks ct [(src^.em_model, rw)          | src <- srcs, rw <- src^.em_rw] tgt


-- DAMAGE APPLICATION

-- apply inflicted damage in a practically correct order
-- control exponential branches by left-associating results
foldWounds :: forall a. Ord a => (a -> Int -> a) -> a -> [Prob TotalDamageWithHooks] -> Prob a
foldWounds f = foldl' consumeRound . return
  where
    consumeRound :: Prob a -> Prob TotalDamageWithHooks -> Prob a
    consumeRound pz pwnd  = do
      TotalDamageWithHooks mhook (TotalWounds (MortalWounds pmw) weaponDmg) <- pwnd

      case mhook of
        Nothing -> let pz' = foldl' consumeWounds pz (SortedList.toAscList weaponDmg)
                   in  pmw >>= \mw -> consumeWounds pz' (Wounds mw (return 1))

        Just (DmgHookMortalWounds pdmgmw) -> do
          (inflictedAnyDamage, z') <- do
              z <- pz
              z' <- foldl' consumeWounds (return z) (SortedList.toAscList weaponDmg)
              return (z /= z', z')

          let pmw' | inflictedAnyDamage = sumProbs [pmw, pdmgmw]
                   | otherwise          = pmw

          pmw' >>= \mw -> consumeWounds (return z') (Wounds mw (return 1))

    consumeWounds :: Prob a -> Wounds DamageRoll -> Prob a
    consumeWounds pz (Wounds n dmg) = foldlProbs' f pz (replicate n dmg)


-- count the number of wounds with a maximum (e.g. until a model is killed) for
-- both efficiency and realism
-- Note: maxWounds <= 0 implies no limit
sumWoundsMax :: Int -> [Prob TotalDamageWithHooks] -> Prob Int
sumWoundsMax maxWounds = top . sumProbs . map (top . sumRound =<<)
  where
    top :: DamageRoll -> DamageRoll
    top | maxWounds > 0 = fmapProbMonotone (min maxWounds)
        | otherwise     = id

    sumTop :: Int -> Int -> Int
    sumTop | maxWounds > 0 = \x y -> min maxWounds (x+y)
           | otherwise     = (+)

    sumRound :: TotalDamageWithHooks -> Prob Int
    sumRound (TotalDamageWithHooks mhook (TotalWounds (MortalWounds pmw) weaponDmg)) = do
        let pdmg = sumProbs [pmw, sumWeaponDmg weaponDmg]

        case mhook of
          Just (DmgHookMortalWounds pdmgmw) -> do
            dmg <- pdmg
            if dmg > 0 then
              fmapProbMonotone (dmg+) pdmgmw
            else
              pdmg

          Nothing -> pdmg

    sumWeaponDmg :: SortedList (Wounds DamageRoll) -> DamageRoll
    sumWeaponDmg =
        sumProbs . map (\(Wounds n pdmg) -> foldAssocIID sumTop 0 n pdmg) . SortedList.toAscList


-- aggregate the number of wounds taken
sumWounds :: [Prob TotalDamageWithHooks] -> Prob Int
sumWounds = sumWoundsMax 0


data WoundedModels = WoundedModels
  { killedModels       :: {-# unpack #-} Int
  , woundedModelWounds :: {-# unpack #-} Int
  }
  deriving (Eq, Ord, Show)


-- fold damage into slain/wounded models
woundModels :: Model -> WoundedModels -> [Prob TotalDamageWithHooks] -> Prob WoundedModels
woundModels tgt = foldWounds woundModels
  where
    woundModels :: WoundedModels -> Int -> WoundedModels
    woundModels (WoundedModels kills wounds) dmg
      | wounds + dmg >= tgt^.model_wnd = WoundedModels (kills+1) 0
      | otherwise                      = WoundedModels kills (wounds + dmg)

-- count wounded models as a fraction of a dead model
fracWoundedModels :: Fractional q => Model -> WoundedModels -> q
fracWoundedModels tgt (WoundedModels kills wounds) =
    fromIntegral kills + fromIntegral wounds / fromIntegral (tgt^.model_wnd)


-- BASIC ANALYSIS

numWounds :: CombatType -> [EquippedModel] -> Model -> Prob Int
numWounds ct srcs tgt =
    sumWounds $ resolveCombat ct srcs tgt

numWoundsMax :: CombatType -> [EquippedModel] -> Model -> Int -> Prob Int
numWoundsMax ct srcs tgt maxWounds =
    sumWoundsMax maxWounds $ resolveCombat ct srcs tgt

resumeNumWoundedModels :: CombatType -> [EquippedModel] -> Model -> WoundedModels -> Prob WoundedModels
resumeNumWoundedModels ct srcs tgt curWounded =
    woundModels tgt curWounded $ resolveCombat ct srcs tgt

numWoundedModels :: CombatType -> [EquippedModel] -> Model -> Prob WoundedModels
numWoundedModels ct srcs tgt =
    resumeNumWoundedModels ct srcs tgt (WoundedModels 0 0)

numSlainModels :: (Ord q, Fractional q) => CombatType -> [EquippedModel] -> Model -> Prob q
numSlainModels ct srcs tgt =
    fmap (fracWoundedModels tgt) $ numWoundedModels ct srcs tgt

numSlainModelsInt :: CombatType -> [EquippedModel] -> Model -> Prob Int
numSlainModelsInt ct srcs =
    fmapProbMonotone (floor :: Double -> Int) . numSlainModels ct srcs

probKill :: CombatType -> [EquippedModel] -> Int -> Model -> QQ
probKill ct srcs 1     tgt = probOf (>= tgt^.model_wnd) (numWoundsMax ct srcs tgt (tgt^.model_wnd))
probKill ct srcs ntgts tgt = probOf (>= ntgts)          (numSlainModelsInt ct srcs tgt)
