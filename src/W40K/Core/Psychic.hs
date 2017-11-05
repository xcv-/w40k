{-# language RebindableSyntax #-}
{-# language TemplateHaskell #-}
module W40K.Core.Psychic where

import Prelude hiding (Functor(..), Applicative(..), Monad(..), liftA2, sequence, (=<<))

import W40K.Core.Prob
import W40K.Core.Mechanics
import Control.Lens

data PsychicPower = PsychicPower
    { _power_castingValue        :: Int
    , _power_inflictMortalWounds :: Model -> Model -> Int -> Prob Int
    }

makeLenses ''PsychicPower

data PsykerCasting = PsykerCasting
    { _cast_bonus                  :: IntMod
    , _cast_usingPsychicChanneling :: Bool
    , _cast_psyker                 :: Model
    , _cast_power                  :: PsychicPower
    }

makeLenses ''PsykerCasting

data PsykerDenying = PsykerDenying
    { _deny_bonus      :: IntMod
    , _deny_usingAegis :: Bool
    }

makeLenses ''PsykerDenying


denyPsychic :: PsykerDenying -> Int -> Prob Bool
denyPsychic deny cv = do
    (a, b) <- if deny^.deny_usingAegis then
                  liftA3 twoHighest d6 d6 d6
              else
                  liftA2 (,) d6 d6

    let denyV = applyIntMod (deny^.deny_bonus) (a + b)
    return (denyV > cv)

castPsychic :: PsykerCasting -> Maybe PsykerDenying -> Model -> Prob Int
castPsychic cast mdeny tgt = do
    (a, b) <- if cast^.cast_usingPsychicChanneling then
                  liftA3 twoHighest d6 d6 d6
              else
                  liftA2 (,) d6 d6

    let minCV = cast^.cast_power.power_castingValue
        cv    = applyIntMod (cast^.cast_bonus) (a + b)

    case (a, b) of
        (1, 1) -> return 0
        (6, 6) -> return 0
        (a, b)
          | cv < minCV -> return 0
          | otherwise  ->
              case mdeny of
                Nothing ->
                    (cast^.cast_power.power_inflictMortalWounds) (cast^.cast_psyker) tgt cv
                Just deny -> do
                    denied <- denyPsychic deny cv
                    if denied then
                        return 0
                    else
                        (cast^.cast_power.power_inflictMortalWounds) (cast^.cast_psyker) tgt cv

castPsychics :: [(PsykerCasting, Maybe PsykerDenying)] -> Model -> Prob Int
castPsychics psykers tgt =
    let mortalWounds = map applyCast psykers
    in  sumProbs mortalWounds
  where
    applyCast (cast, deny) = castPsychic cast deny tgt
