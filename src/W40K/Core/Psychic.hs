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
    }

makeLenses ''PsykerCasting

data PsykerDenying = PsykerDenying
    { _deny_bonus      :: IntMod
    , _deny_usingAegis :: Bool
    }

makeLenses ''PsykerDenying


psychicTest :: PsykerCasting -> PsychicPower -> Prob (Maybe Int)
psychicTest caster power = do
    (a, b) <- if caster^.cast_usingPsychicChanneling then
                  liftA3 twoHighest d6 d6 d6
              else
                  liftA2 (,) d6 d6

    let minCV = power^.power_castingValue
        cv    = applyIntMod (caster^.cast_bonus) (a + b)

    case (a, b) of
        (1, 1) -> return Nothing
        (6, 6) -> return Nothing
        (a, b)
          | cv < minCV -> return Nothing
          | otherwise  -> return (Just cv)

denyTest :: PsykerDenying -> Int -> Prob Bool
denyTest deny cv = do
    (a, b) <- if deny^.deny_usingAegis then
                  liftA3 twoHighest d6 d6 d6
              else
                  liftA2 (,) d6 d6

    let denyV = applyIntMod (deny^.deny_bonus) (a + b)
    return (denyV > cv)

manifestPower :: PsykerCasting -> PsychicPower -> Maybe PsykerDenying -> Prob (Maybe Int)
manifestPower caster power Nothing       = psychicTest caster power
manifestPower caster power (Just deny) = do
    mcv <- psychicTest caster power

    case mcv of
        Nothing -> return Nothing
        Just cv -> do
            denied <- denyTest deny cv
            return (if denied then Nothing else Just cv)


castPsychic :: PsykerCasting -> PsychicPower -> Maybe PsykerDenying -> Model -> Prob Int
castPsychic caster power mdeny tgt = do
    mcv <- manifestPower caster power mdeny
    case mcv of
        Nothing -> return 0
        Just cv -> (power^.power_inflictMortalWounds) (caster^.cast_psyker) tgt cv

castPsychics :: [(PsykerCasting, PsychicPower, Maybe PsykerDenying)] -> Model -> Prob Int
castPsychics psykers tgt =
    let mortalWounds = map applyCast psykers
    in  sumProbs mortalWounds
  where
    applyCast (caster, power, deny) = castPsychic caster power deny tgt
