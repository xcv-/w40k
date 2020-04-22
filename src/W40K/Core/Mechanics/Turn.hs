{-# language ExistentialQuantification #-}
{-# language RankNTypes #-}
{-# language RebindableSyntax #-}
{-# language Strict #-}
{-# language TemplateHaskell #-}
module W40K.Core.Mechanics.Turn
  ( Turn(..)
  , turnPsychicPhase
  , turnShootingPhase
  , turnChargePhase
  , turnFightPhase
  , emptyTurn

  , GenericTurn(..)
  , eraseTurn
  , mapGenericTurn

  , TurnLenses
  , turnName
  , turnTarget
  , turnShooting
  , turnFighting
  , turnAttacks
  , turnShooting_
  , turnFighting_
  , turnAttacks_

  , turnNumWounds
  , turnNumWoundsMax
  , turnNumSlainModels
  , turnNumSlainModelsInt
  , turnProbKill
  ) where

import Prelude hiding (Functor(..), Monad(..), (=<<))
import qualified Prelude

import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Prob

import W40K.Core.Mechanics.Attack
import W40K.Core.Mechanics.Equipped
import W40K.Core.Mechanics.Model


-- TURN MECHANICS

data Turn pr cr tgt = Turn
  { _turnName :: String

  -- phases can adapt to the target Model
  , _turnPsychicPhase  :: tgt -> Prob (MortalWounds, pr)
  , _turnShootingPhase :: pr -> tgt -> [EquippedModel]
  , _turnChargePhase   :: pr -> tgt -> Prob cr
  , _turnFightPhase    :: pr -> cr -> tgt -> [EquippedModel]
  }

makeLensesFor
  [ ("_turnPsychicPhase",  "turnPsychicPhase")
  , ("_turnShootingPhase", "turnShootingPhase")
  , ("_turnChargePhase",   "turnChargePhase")
  , ("_turnFightPhase",    "turnFightPhase")
  ]
  ''Turn


emptyTurn :: Turn () () tgt
emptyTurn = Turn "" (\_ -> return (mempty, ())) (\_ _ -> []) (\_ _ -> return ()) (\_ _ _ -> [])


interleaveTurns :: (Ord pr1, Ord cr1, Ord pr2, Ord cr2)
    => Turn pr1 cr1 tgt
    -> Turn pr2 cr2 tgt
    -> Turn (pr1, pr2) (cr1, cr2) tgt
interleaveTurns turn1 turn2 =
    Turn {
      _turnName =
        case (_turnName turn1, _turnName turn2) of
          (n1, "") -> n1
          ("", n2) -> n2
          (n1, n2) -> n1 ++ " + " ++ n2,

      _turnPsychicPhase = \tgt ->
          liftA2 (\(mw1, pr1) (mw2, pr2) -> (mw1 <> mw2, (pr1, pr2)))
                 (_turnPsychicPhase turn1 tgt)
                 (_turnPsychicPhase turn2 tgt),

      _turnShootingPhase = \(pr1, pr2) tgt ->
          _turnShootingPhase turn1 pr1 tgt ++ _turnShootingPhase turn2 pr2 tgt,

      _turnChargePhase = \(pr1, pr2) tgt ->
          liftA2 (,) (_turnChargePhase turn1 pr1 tgt) (_turnChargePhase turn2 pr2 tgt),

      _turnFightPhase = \(pr1, pr2) (cr1, cr2) tgt ->
          _turnFightPhase turn1 pr1 cr1 tgt ++ _turnFightPhase turn2 pr2 cr2 tgt
    }


data GenericTurn tgt = forall pr cr. (Ord pr, Ord cr) => GenericTurn (Turn pr cr tgt)


eraseTurn :: (Ord pr, Ord cr) => Turn pr cr tgt -> GenericTurn tgt
eraseTurn = GenericTurn


mapGenericTurn ::
    (forall pr cr. (Ord pr, Ord cr) => Turn pr cr tgt -> Turn pr cr tgt')
    -> GenericTurn tgt
    -> GenericTurn tgt'
mapGenericTurn f (GenericTurn t) = GenericTurn (f t)


instance Semigroup (GenericTurn tgt) where
  GenericTurn turn1 <> GenericTurn turn2 = eraseTurn $ interleaveTurns turn1 turn2

instance Monoid (GenericTurn tgt) where
  mempty = eraseTurn emptyTurn


class TurnLenses turn where
  turnName      :: Lens' (turn tgt) String
  turnTarget    :: Setter (turn tgt) (turn tgt') tgt' tgt
  turnShooting  :: Setter' (turn tgt) (tgt -> [EquippedModel])
  turnFighting  :: Setter' (turn tgt) (tgt -> [EquippedModel])


turnShooting_ :: TurnLenses turn => Setter' (turn tgt) [EquippedModel]
turnShooting_ = turnShooting.mapped

turnFighting_ :: TurnLenses turn => Setter' (turn tgt) [EquippedModel]
turnFighting_ = turnFighting.mapped

turnAttacks :: TurnLenses turn => Setter' (turn tgt) (tgt -> [EquippedModel])
turnAttacks = sets $ \f -> over turnShooting f . over turnFighting f

turnAttacks_ :: TurnLenses turn => Setter' (turn tgt) [EquippedModel]
turnAttacks_ = sets $ \f -> over turnShooting_ f . over turnFighting_ f


instance TurnLenses (Turn pr cr) where
  turnName f t = Prelude.fmap (\name' -> t { _turnName = name' }) $ f (_turnName t)

  turnTarget = sets $ \f t -> t
    { _turnPsychicPhase  = \tgt       -> _turnPsychicPhase t (f tgt)
    , _turnShootingPhase = \pr tgt    -> _turnShootingPhase t pr (f tgt)
    , _turnChargePhase   = \pr tgt    -> _turnChargePhase t pr (f tgt)
    , _turnFightPhase    = \pr cr tgt -> _turnFightPhase t pr cr (f tgt)
    }

  turnShooting = sets $ \f t -> t { _turnShootingPhase = f . _turnShootingPhase t }
  turnFighting = sets $ \f t -> t { _turnFightPhase = \pr -> f . _turnFightPhase t pr }


instance TurnLenses GenericTurn where
  turnName f (GenericTurn t) = Prelude.fmap GenericTurn (turnName f t)

  turnTarget = sets $ \f -> mapGenericTurn (over turnTarget f)

  turnShooting = sets $ \f -> mapGenericTurn (over turnShooting f)
  turnFighting = sets $ \f -> mapGenericTurn (over turnFighting f)


turnNumWounds :: (AsModel tgt, Ord pr, Ord cr) => Turn pr cr tgt -> tgt -> Prob Int
turnNumWounds turn tgt = do
    (mw, pr) <- _turnPsychicPhase turn tgt
    let psydmg = TotalDamageWithHooks Nothing (effectiveMortalWounds tgtModel mw)
    cr <- _turnChargePhase turn pr tgt

    sumProbs [sumWounds [return psydmg],
              numWounds Ranged (_turnShootingPhase turn pr tgt) tgtModel,
              numWounds Melee  (_turnFightPhase turn pr cr tgt) tgtModel]
  where
    tgtModel = tgt^.as_model


turnNumWoundsMax :: (AsModel tgt, Ord pr, Ord cr) => Turn pr cr tgt -> tgt -> Prob Int
turnNumWoundsMax turn tgt = do
    (mw, pr) <- _turnPsychicPhase turn tgt
    let psydmg = TotalDamageWithHooks Nothing (effectiveMortalWounds tgtModel mw)
    cr <- _turnChargePhase turn pr tgt

    sumProbs [sumWoundsMax tgtWounds [return psydmg],
              numWoundsMax Ranged (_turnShootingPhase turn pr tgt) tgtModel tgtWounds,
              numWoundsMax Melee  (_turnFightPhase turn pr cr tgt) tgtModel tgtWounds]
      & fmapProbMonotone (min tgtWounds)
  where
    tgtModel = tgt^.as_model
    tgtWounds = tgt^.as_model.model_wnd


turnNumSlainModels :: (AsModel tgt, Ord pr, Ord cr) => Turn pr cr tgt -> tgt -> Prob QQ
turnNumSlainModels turn tgt = do
    (mw, pr) <- _turnPsychicPhase turn tgt
    let psydmg = TotalDamageWithHooks Nothing (effectiveMortalWounds tgtModel mw)
    cr <- _turnChargePhase turn pr tgt

    fmapProbMonotone (fracWoundedModels tgtModel) $
      woundModels tgtModel (WoundedModels 0 0) [return psydmg]
        >>= resumeNumWoundedModels Ranged (_turnShootingPhase turn pr tgt) tgtModel
        >>= resumeNumWoundedModels Melee  (_turnFightPhase turn pr cr tgt) tgtModel
  where
    tgtModel = tgt^.as_model


turnNumSlainModelsInt :: (AsModel tgt, Ord pr, Ord cr) => Turn pr cr tgt -> tgt -> Prob Int
turnNumSlainModelsInt turn = fmapProbMonotone floor . turnNumSlainModels turn


turnProbKill :: (AsModel tgt, Ord pr, Ord cr) => Turn pr cr (UnitOf tgt) -> UnitOf tgt -> QQ
turnProbKill turn (UnitOf n tgt) =
    case n of
      1 -> probOf (>= tgtWounds) (turnNumWoundsMax      (turn & turnTarget %~ UnitOf 1) tgt)
      _ -> probOf (>= n)         (turnNumSlainModelsInt (turn & turnTarget %~ UnitOf n) tgt)
  where
    tgtWounds = tgt^.as_model.model_wnd
