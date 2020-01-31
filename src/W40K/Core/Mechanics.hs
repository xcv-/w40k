{-# language BangPatterns #-}
{-# language ExistentialQuantification #-}
{-# language RankNTypes #-}
{-# language RebindableSyntax #-}
{-# language ScopedTypeVariables #-}
{-# language Strict #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module W40K.Core.Mechanics
  ( module Mechanics
  , MortalWounds(..)
  , TotalDamage
  , sumWounds
  , sumWoundsMax
  , WoundedModels(..)
  , woundModels
  , fracWoundedModels
  , EquippedModel(..)
  , em_model, em_ccw, em_rw, em_name, em_weapons
  , basicEquippedModel
  , attackSplit
  , Modifier
  , with
  , applyAura
  , within

  , numWounds
  , numWoundsMax
  , resumeNumWoundedModels
  , numWoundedModels
  , numSlainModels
  , probKill

  , Turn(..)
  , emptyTurn
  , GenericTurn(..)
  , eraseTurn
  , mapGenericTurn
  , TurnLenses
  , turnName
  , turnShooting
  , turnMelee
  , turnAttacks
  , turnNumWounds
  , turnNumWoundsMax
  , turnNumSlainModels
  , turnNumSlainModelsInt
  , turnProbKill
  ) where

import Prelude hiding (Functor(..), Monad(..), (=<<))
import qualified Prelude

import Control.Lens

import Data.Function    (on)
import Data.List        (foldl', isInfixOf, sortBy)
import Data.Traversable (for)

import W40K.Core.SortedList (SortedList, SortedListItem(..))
import qualified W40K.Core.SortedList as SortedList

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Util (parMap)
import W40K.Core.Mechanics.Common as Mechanics
import W40K.Core.Mechanics.Melee  as Mechanics
import W40K.Core.Mechanics.Ranged as Mechanics


-- GENERAL CORE MECHANICS

newtype MortalWounds = MortalWounds { getMortalWounds :: Prob Int }
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


type DamageRoll = Prob Int
type TotalDamage = TotalWounds DamageRoll

data WeaponTotalDamage = WeaponTotalDamage
  { _weapondmg_hook   :: !(Maybe DmgHook)
  , _weapondmg_wounds :: !TotalDamage
  }
  deriving (Eq, Ord, Show)

makeLenses ''WeaponTotalDamage

satisfiesReqRoll :: RequiredRoll -> SkillRoll -> Bool
satisfiesReqRoll (MinModifiedRoll m)   r = modifiedRoll r >= m
satisfiesReqRoll (MinUnmodifiedRoll m) r = unmodifiedRoll r >= m

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
    mods = woundMods src w

    withMult !n !p = (n, p)


resolveHooks :: Show a => (eff -> [RollHook eff] -> Prob a) -> SkillRoll -> [RollHook eff] -> [Prob a]
resolveHooks resolveEff r = go r []
  where
    go _ _         []                                  = []
    go r prevHooks (hook@(RollHook req eff):nextHooks)
      | satisfiesReqRoll req r = resolveEff eff (prevHooks ++ nextHooks) : go r (hook:prevHooks) nextHooks
      | otherwise              = go r (hook:prevHooks) nextHooks

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

unsavedDamage :: IsWeapon w => Model -> w -> Model -> Prob WeaponTotalDamage
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

    fmap (WeaponTotalDamage (w^.as_weapon.w_hooks.hook_dmg))
         (foldIID mult unsavedp)

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


effectiveMortalWounds :: Model -> MortalWounds -> TotalWounds a
effectiveMortalWounds tgt (MortalWounds mw) = TotalWounds (MortalWounds (dmgAfterFnp mw)) SortedList.empty
  where
    dmgAfterFnp :: DamageRoll -> DamageRoll
    dmgAfterFnp pdmg = applyFnp (tgt^.model_fnp) =<< pdmg

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


evaluateAttack :: IsWeapon w => CombatType -> Model -> w -> Model -> Prob WeaponTotalDamage
evaluateAttack ct src w tgt = fmap (weapondmg_wounds %~ effectiveDamage ct tgt) (unsavedDamage src w tgt)


evaluateAttacks :: IsWeapon w => CombatType -> [(Model, w)] -> Model -> [Prob WeaponTotalDamage]
evaluateAttacks ct srcws tgt = concat $
    parMap (\(cnt, src, w) -> replicate cnt $! evaluateAttack ct src w tgt)
           (shrinkModels srcws)


foldWounds :: forall a. Ord a => (a -> Int -> a) -> a -> [Prob WeaponTotalDamage] -> Prob a
foldWounds f = foldl' consumeRound . return
  where
    consumeRound :: Prob a -> Prob WeaponTotalDamage -> Prob a
    consumeRound pz pwnd  = do
      WeaponTotalDamage mhook (TotalWounds (MortalWounds pmw) weaponDmg) <- pwnd

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


sumWoundsMax :: Int -> [Prob WeaponTotalDamage] -> Prob Int
sumWoundsMax maxWounds = top . sumProbs . map (top . sumRound =<<)
  where
    top :: DamageRoll -> DamageRoll
    top | maxWounds > 0 = fmapProbMonotone (min maxWounds)
        | otherwise     = id

    sumTop :: Int -> Int -> Int
    sumTop | maxWounds > 0 = \x y -> min maxWounds (x+y)
           | otherwise     = (+)

    sumRound :: WeaponTotalDamage -> Prob Int
    sumRound (WeaponTotalDamage mhook (TotalWounds (MortalWounds pmw) weaponDmg)) = do
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

sumWounds :: [Prob WeaponTotalDamage] -> Prob Int
sumWounds = sumWoundsMax 0


data WoundedModels = WoundedModels
  { killedModels       :: {-# unpack #-} !Int
  , woundedModelWounds :: {-# unpack #-} !Int
  } deriving (Eq, Ord, Show)

woundModels :: Model -> WoundedModels -> [Prob WeaponTotalDamage] -> Prob WoundedModels
woundModels tgt = foldWounds woundModels
  where
    woundModels :: WoundedModels -> Int -> WoundedModels
    woundModels (WoundedModels kills wounds) dmg
      | wounds + dmg >= tgt^.model_wnd = WoundedModels (kills+1) 0
      | otherwise                      = WoundedModels kills (wounds + dmg)

fracWoundedModels :: Model -> WoundedModels -> QQ
fracWoundedModels tgt (WoundedModels kills wounds) =
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

em_weapons :: Traversal' EquippedModel Weapon
em_weapons f (EquippedModel m ccw rw) =
    EquippedModel m <$> as_weapon f ccw <*> (traverse.as_weapon) f rw

basicEquippedModel :: Model -> EquippedModel
basicEquippedModel model = EquippedModel model basic_ccw []

attackSplit :: Int -> CCWeapon -> EquippedModel -> [EquippedModel]
attackSplit n ccw em =
    [ em & em_model.model_att -~ n
    , em & em_model.model_att .~ n
         & em_ccw .~ ccw
    ]

evaluateCombat :: CombatType -> [EquippedModel] -> Model -> [Prob WeaponTotalDamage]
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


-- BASIC ANALYSIS

numWounds :: CombatType -> [EquippedModel] -> Model -> Prob Int
numWounds ct srcs tgt = sumWounds $ evaluateCombat ct srcs tgt

numWoundsMax :: CombatType -> [EquippedModel] -> Model -> Int -> Prob Int
numWoundsMax ct srcs tgt maxWounds = sumWoundsMax maxWounds $ evaluateCombat ct srcs tgt

resumeNumWoundedModels :: CombatType -> [EquippedModel] -> Model -> WoundedModels -> Prob WoundedModels
resumeNumWoundedModels ct srcs tgt curWounded = woundModels tgt curWounded $ evaluateCombat ct srcs tgt

numWoundedModels :: CombatType -> [EquippedModel] -> Model -> Prob WoundedModels
numWoundedModels ct srcs tgt = resumeNumWoundedModels ct srcs tgt (WoundedModels 0 0)

numSlainModels :: CombatType -> [EquippedModel] -> Model -> Prob QQ
numSlainModels ct srcs tgt = fmap (fracWoundedModels tgt) $ numWoundedModels ct srcs tgt

numSlainModelsInt :: CombatType -> [EquippedModel] -> Model -> Prob Int
numSlainModelsInt ct srcs = fmapProbMonotone floor . numSlainModels ct srcs

probKill :: CombatType -> [EquippedModel] -> Int -> Model -> QQ
probKill ct srcs 1     tgt = probOf (>= tgt^.model_wnd) (numWoundsMax ct srcs tgt (tgt^.model_wnd))
probKill ct srcs ntgts tgt = probOf (>= ntgts)          (numSlainModelsInt ct srcs tgt)


-- TURN MECHANICS

data Turn pr cr = Turn
  { _turnName     :: String
  , _turnPsychic  :: Model -> Prob (MortalWounds, pr)
  , _turnShooting :: pr -> [EquippedModel]
  , _turnCharges  :: pr -> Prob cr
  , _turnMelee    :: pr -> cr -> [EquippedModel]
  }


emptyTurn :: Turn () ()
emptyTurn = Turn "" (\_ -> return (mempty, ())) (\_ -> []) (\_ -> return ()) (\_ _ -> [])

interleaveTurns :: (Ord pr1, Ord cr1, Ord pr2, Ord cr2) => Turn pr1 cr1 -> Turn pr2 cr2 -> Turn (pr1, pr2) (cr1, cr2)
interleaveTurns turn1 turn2 =
    Turn {
      _turnName =
        case (_turnName turn1, _turnName turn2) of
          (n1, "") -> n1
          ("", n2) -> n2
          (n1, n2) -> n1 ++ " + " ++ n2,

      _turnPsychic = \tgt -> liftA2 (\(mw1, pr1) (mw2, pr2) -> (mw1 <> mw2, (pr1, pr2)))
                                    (_turnPsychic turn1 tgt)
                                    (_turnPsychic turn2 tgt),

      _turnCharges = \(pr1, pr2) -> liftA2 (,) (_turnCharges turn1 pr1) (_turnCharges turn2 pr2),

      _turnShooting = \(pr1, pr2)            -> _turnShooting turn1 pr1  ++ _turnShooting turn2 pr2,
      _turnMelee    = \(pr1, pr2) (cr1, cr2) -> _turnMelee turn1 pr1 cr1 ++ _turnMelee turn2 pr2 cr2
    }

data GenericTurn = forall pr cr. (Ord pr, Ord cr) => GenericTurn (Turn pr cr)

eraseTurn :: (Ord pr, Ord cr) => Turn pr cr -> GenericTurn
eraseTurn = GenericTurn

mapGenericTurn :: (forall pr cr. (Ord pr, Ord cr) => Turn pr cr -> Turn pr cr) -> GenericTurn -> GenericTurn
mapGenericTurn f (GenericTurn t) = GenericTurn (f t)

instance Semigroup GenericTurn where
  GenericTurn turn1 <> GenericTurn turn2 = eraseTurn $ interleaveTurns turn1 turn2

instance Monoid GenericTurn where
  mempty = eraseTurn emptyTurn


class TurnLenses turn where
  turnName     :: Lens' turn String
  turnShooting :: Setter' turn [EquippedModel]
  turnMelee    :: Setter' turn [EquippedModel]

turnAttacks :: TurnLenses turn => Setter' turn [EquippedModel]
turnAttacks = sets $ \f -> over turnShooting f . over turnMelee f

instance TurnLenses (Turn pr cr) where
  turnName f t = Prelude.fmap (\name' -> t { _turnName = name' }) $ f (_turnName t)
  turnShooting = sets $ \f t -> t { _turnShooting = f . _turnShooting t }
  turnMelee    = sets $ \f t -> t { _turnMelee    = \pr -> f . _turnMelee t pr }

instance TurnLenses GenericTurn where
  turnName f (GenericTurn t) = Prelude.fmap GenericTurn (turnName f t)
  turnShooting = sets $ \f -> mapGenericTurn (over turnShooting f)
  turnMelee    = sets $ \f -> mapGenericTurn (over turnMelee f)


turnNumWounds :: (Ord pr, Ord cr) => Turn pr cr -> Model -> Prob Int
turnNumWounds turn tgt = do
    (mw, pr) <- _turnPsychic turn tgt
    let psydmg = WeaponTotalDamage Nothing (effectiveMortalWounds tgt mw)
    cr <- _turnCharges turn pr

    sumProbs [sumWounds [return psydmg],
              numWounds Ranged (_turnShooting turn pr) (tgt^.as_model),
              numWounds Melee  (_turnMelee turn pr cr) (tgt^.as_model)]

turnNumWoundsMax :: (Ord pr, Ord cr) => Turn pr cr -> Model -> Int -> Prob Int
turnNumWoundsMax turn tgt maxWounds = do
    (mw, pr) <- _turnPsychic turn tgt
    let psydmg = WeaponTotalDamage Nothing (effectiveMortalWounds tgt mw)
    cr <- _turnCharges turn pr

    sumProbs [sumWoundsMax maxWounds [return psydmg],
              numWoundsMax Ranged (_turnShooting turn pr) tgt maxWounds,
              numWoundsMax Melee  (_turnMelee turn pr cr) tgt maxWounds]
      & fmapProbMonotone (min maxWounds)

turnNumSlainModels :: (Ord pr, Ord cr) => Turn pr cr -> Model -> Prob QQ
turnNumSlainModels turn tgt = do
    (mw, pr) <- _turnPsychic turn tgt
    let psydmg = WeaponTotalDamage Nothing (effectiveMortalWounds tgt mw)
    cr <- _turnCharges turn pr

    fmapProbMonotone (fracWoundedModels tgt) $
      woundModels tgt (WoundedModels 0 0) [return psydmg]
        >>= resumeNumWoundedModels Ranged (_turnShooting turn pr) tgt
        >>= resumeNumWoundedModels Melee  (_turnMelee turn pr cr) tgt

turnNumSlainModelsInt :: (Ord pr, Ord cr) => Turn pr cr -> Model -> Prob Int
turnNumSlainModelsInt turn = fmapProbMonotone floor . turnNumSlainModels turn

turnProbKill :: (Ord pr, Ord cr) => Turn pr cr -> Int -> Model -> QQ
turnProbKill turn 1     tgt = probOf (>= tgt^.model_wnd) (turnNumWoundsMax turn tgt (tgt^.model_wnd))
turnProbKill turn ntgts tgt = probOf (>= ntgts)          (turnNumSlainModelsInt turn tgt)
