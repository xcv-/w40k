{-# language RebindableSyntax #-}
{-# language TemplateHaskell #-}
module W40K.Core.Psychic where

import Prelude hiding (Functor(..), Applicative(..), Monad(..), liftA2, sequence, (=<<))

import Data.List (sort)
import Data.Maybe (isJust)

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics
import Control.Lens

data PsychicPower = PsychicPower
  { _power_castingValue        :: Int
  , _power_inflictMortalWounds :: Model -> Model -> Int -> Prob Int
  , _power_mod                 :: Modifier
  }

makeLenses ''PsychicPower

data Psyker = Psyker
  { _psyker_cast_roll :: Prob [Int]
  , _psyker_deny_roll :: Prob [Int]
  , _psyker_cast_mod  :: IntMod
  , _psyker_deny_mod  :: IntMod
  }

makeLenses ''Psyker


psychicTest :: Psyker -> PsychicPower -> Prob (Maybe Int)
psychicTest caster power = do
    rolls <- caster^.psyker_cast_roll

    let minCV = power^.power_castingValue
        cv    = applyIntMod (caster^.psyker_cast_mod) (sum rolls)

    case sort rolls of
      1:1:_ -> return Nothing
      _ | cv < minCV -> return Nothing
        | otherwise  -> return (Just cv)

denyTest :: Psyker -> Int -> Prob Bool
denyTest denier cv = do
    rolls <- denier^.psyker_deny_roll
    let denyV = applyIntMod (denier^.psyker_deny_mod) (sum rolls)

    return (denyV > cv)

manifestPower :: Psyker -> PsychicPower -> Maybe Psyker -> Prob (Maybe Int)
manifestPower caster power Nothing       = psychicTest caster power
manifestPower caster power (Just denier) = do
    mcv <- psychicTest caster power

    case mcv of
        Nothing -> return Nothing
        Just cv -> do
            denied <- denyTest denier cv
            return (if denied then Nothing else Just cv)

doesManifestPower :: Psyker -> PsychicPower -> Maybe Psyker -> Prob Bool
doesManifestPower caster power mdeny = fmapProb isJust (manifestPower caster power mdeny)


castOffensivePsychic :: Psyker -> PsychicPower -> Maybe Psyker -> Model -> Model -> Prob Int
castOffensivePsychic caster power mdeny src tgt = do
    mcv <- manifestPower caster power mdeny
    case mcv of
        Nothing -> return 0
        Just cv -> (power^.power_inflictMortalWounds) src tgt cv

castOffensivePsychics :: [(Psyker, PsychicPower, Maybe Psyker, Model)] -> Model -> Prob Int
castOffensivePsychics psykers tgt =
    let mortalWounds = map applyCast psykers
    in  sumProbs mortalWounds
  where
    applyCast (caster, power, denier, src) = castOffensivePsychic caster power denier src tgt
