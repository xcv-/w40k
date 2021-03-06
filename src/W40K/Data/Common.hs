{-# language RebindableSyntax #-}
module W40K.Data.Common where

import Prelude hiding (Functor(..), Monad(..), sequence)
import Data.Bool (bool)
import Data.List (intercalate)
import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics


-- TOOLS AND MODIFIERS

closeEnough :: Effect
closeEnough = rapidFiring . meltaRange

twice :: (Ord a, Num a) => Prob a -> Prob a
twice = sumIID 2

thrice :: (Ord a, Num a) => Prob a -> Prob a
thrice = sumIID 3

twin :: RngWeapon -> RngWeapon
twin = (rw_shots %~ twice) . (rw_name %~ ("twin " ++))

quad :: RngWeapon -> RngWeapon
quad = (rw_shots %~ sumIID 4) . (rw_name %~ ("quad " ++))

two :: Semigroup s => s -> s
two s = s <> s

twoHighest :: Ord a => a -> a -> a -> (a, a)
twoHighest a b c
  | a <= b && a <= c = (b, c)
  | b <= a && b <= c = (a, c)
  | otherwise        = (a, b)

successiveRollMortalWounds :: Int -> Prob Int
successiveRollMortalWounds start
  | start > 6 = return 0
  | otherwise = do
      success <- bernoulli (prob_d6_geq start)

      if success then
        fmapProbMonotone succ (successiveRollMortalWounds (start+1))
      else
        return 0

weaponNames :: [RngWeapon] -> String
weaponNames rw = intercalate "+" (rw^..traverse.rw_name)


-- GENERIC TURN FLOWS

psychicEffect :: Psyker -> PsychicPower -> (tgt -> Maybe Psyker) -> GenericTurn tgt -> GenericTurn tgt
psychicEffect caster power denier (GenericTurn turn) =
    eraseTurn turn {
      _turnPsychicPhase = \tgt -> do
          succeed <- doesManifestPower caster power (denier tgt)
          (mw0, pr) <- _turnPsychicPhase turn tgt
          return (mw0, (pr, succeed)),

      _turnShootingPhase = \(pr, succeed) tgt ->
          if succeed then
            with [power^.power_effect] (_turnShootingPhase turn pr tgt)
          else
            _turnShootingPhase turn pr tgt,

      _turnChargePhase = \(pr, _) tgt -> _turnChargePhase turn pr tgt,

      _turnFightPhase = \(pr, succeed) cr tgt ->
          if succeed then
            with [power^.power_effect] (_turnFightPhase turn pr cr tgt)
          else
            _turnFightPhase turn pr cr tgt
    }

chargeFilter :: Prob Bool -> GenericTurn tgt -> GenericTurn tgt
chargeFilter pCharge (GenericTurn turn) =
    eraseTurn turn {
      _turnChargePhase = \pr tgt -> do
        cr <- _turnChargePhase turn pr tgt
        crFilter <- pCharge
        return (cr, crFilter),

      _turnFightPhase = \pr (cr, crFilter) tgt ->
          if crFilter then
            _turnFightPhase turn pr cr tgt
          else
            []
    }

deepstrikeCharge :: ChargeRerolls -> Int -> GenericTurn tgt -> GenericTurn tgt
deepstrikeCharge rr chargeMod gt = gt
    & chargeFilter (chargeRoll rr (9-chargeMod))
    & turnShooting_ %~ with [moving]


-- MODELS

nosave :: Int
nosave = 9999

stormShield :: Model -> Model
stormShield m = m
  & model_inv  .~ 3
  & model_name <>~ " (shield)"


meq :: Model
meq = Model
  { _model_class              = Infantry
  , _model_ws                 = 3
  , _model_bs                 = 3
  , _model_str                = 4
  , _model_tgh                = 4
  , _model_att                = 1
  , _model_wnd                = 1
  , _model_ld                 = 7
  , _model_save               = 3
  , _model_cc_inv             = nosave
  , _model_rng_inv            = nosave
  , _model_moved              = False
  , _model_cc_mods            = noMods
  , _model_rng_mods           = noMods
  , _model_quantumShielding   = False
  , _model_allIsDust          = False
  , _model_ignoreHeavy        = False
  , _model_unmodifiedMinWound = 1
  , _model_fnp                = 7
  , _model_name               = "MEQ"
  }

geq :: Model
geq = meq
  & model_ws    .~ 4
  & model_bs    .~ 4
  & model_str   .~ 3
  & model_tgh   .~ 3
  & model_ld    .~ 6
  & model_save  .~ 5
  & model_name  .~ "GEQ"

teq :: Model
teq = meq
  & model_att    .~ 2
  & model_wnd    .~ 2
  & model_ld     .~ 8
  & model_save   .~ 2
  & model_inv    .~ 5
  & model_name   .~ "TEQ"

rhino :: Model
rhino = meq
  & model_class  .~ Vehicle
  & model_ws     .~  6
  & model_str    .~  6
  & model_tgh    .~  7
  & model_att    .~  3
  & model_wnd    .~ 10
  & model_ld     .~  8
  & model_save   .~  3
  & model_name   .~ "rhino"


-- PSYCHIC

defaultPsyker :: Psyker
defaultPsyker = Psyker (sequence [d6,d6]) (sequence [d6,d6]) NoMod NoMod


effectPsychic :: Int -> Effect -> PsychicPower
effectPsychic wc eff = PsychicPower
  { _power_castingValue = wc
  , _power_inflictMortalWounds = \_ _ _ -> mempty
  , _power_effect = eff
  }


offensivePsychic :: Int -> (Model -> Model -> Int -> MortalWounds) -> PsychicPower
offensivePsychic wc dmg = PsychicPower
  { _power_castingValue = wc
  , _power_inflictMortalWounds = dmg
  , _power_effect = id
  }


smite :: PsychicPower
smite = offensivePsychic 5 $ \_ _ v ->
    if v > 10 then
      MortalWounds d6
    else
      MortalWounds d3


-- RANGED WEAPONS

bolter :: RngWeapon
bolter = RngWeapon
  { _rw_shots   = return 1
  , _rw_str     = 4
  , _rw_class   = RapidFire
  , _rw_melta   = False
  , _rw_weapon  = basicWeapon "boltgun"
  }

boltPistol :: RngWeapon
boltPistol = bolter
  & rw_class .~ Pistol
  & rw_name  .~ "bolt pistol"

stormBolter :: RngWeapon
stormBolter = bolter
  & rw_shots  .~ return 2
  & rw_name   .~ "storm bolter"

hurricaneBolter :: RngWeapon
hurricaneBolter = bolter
  & rw_shots  .~ return 6
  & rw_name   .~ "hurricane bolter"

heavyStubber :: RngWeapon
heavyStubber = bolter
  & rw_shots .~ return 3
  & rw_class .~ Heavy
  & rw_name  .~ "heavy stubber"

heavyBolter :: RngWeapon
heavyBolter = bolter
  & rw_shots .~ return 3
  & rw_class .~ Heavy
  & rw_str   .~ 5
  & rw_ap    .~ -1
  & rw_name  .~ "heavy bolter"

assaultCannon :: RngWeapon
assaultCannon = heavyBolter
  & rw_shots .~ return 6
  & rw_str   .~ 6
  & rw_name  .~ "assault cannon"

autocannon :: RngWeapon
autocannon = heavyBolter
  & rw_shots .~ return 2
  & rw_str   .~ 7
  & rw_ap    .~ -1
  & rw_dmg   .~ return 2
  & rw_name  .~ "autocannon"

lascannon :: RngWeapon
lascannon = RngWeapon
  { _rw_shots   = return 1
  , _rw_str     = 9
  , _rw_class   = Heavy
  , _rw_melta   = False
  , _rw_weapon  = basicWeapon "lascannon"
    & w_ap     .~ -3
    & w_dmg    .~ d6
  }

plasmaPistol :: Bool -> RngWeapon
plasmaPistol overcharge = boltPistol
  & rw_class .~ Pistol
  & rw_str   .~ bool 8 7 overcharge
  & rw_ap    .~ -3
  & rw_dmg   .~ return (bool 2 1 overcharge)
  & rw_name  .~ "plasma pistol"

plasmaCannon :: Bool -> RngWeapon
plasmaCannon overcharge = bolter
  & rw_class .~ Heavy
  & rw_shots .~ d3
  & rw_str   .~ bool 8 7 overcharge
  & rw_ap    .~ -3
  & rw_dmg   .~ return (bool 2 1 overcharge)
  & rw_name  .~ "plasma cannon"


flamer :: RngWeapon
flamer = bolter
  & rw_class            .~ Assault
  & rw_shots            .~ d6
  & rw_weapon.w_autohit .~ True
  & rw_name             .~ "flamer"

heavyFlamer :: RngWeapon
heavyFlamer = flamer
  & rw_class   .~ Heavy
  & rw_str     .~ 5
  & rw_ap      .~ -1
  & rw_name    .~ "heavy flamer"

meltagun :: RngWeapon
meltagun = bolter
  & rw_class  .~ Assault
  & rw_str    .~ 8
  & rw_ap     .~ -4
  & rw_melta  .~ True
  & rw_name   .~ "meltagun"

multimelta :: RngWeapon
multimelta = meltagun
  & rw_class  .~ Heavy
  & rw_name   .~ "multi-melta"


krakMissile :: RngWeapon
krakMissile = lascannon
  & rw_str   .~ 8
  & rw_ap    .~ -2
  & rw_name  .~ "krak missile"


-- CC WEAPONS

chainsword :: CCWeapon
chainsword = basic_ccw
  & ccw_attBonus .~ Add 1
  & ccw_name     .~ "chainsword"

powerSword :: CCWeapon
powerSword = basic_ccw
  & ccw_ap   .~ -3
  & ccw_name .~ "power sword"

forceSword :: CCWeapon
forceSword = powerSword
  & ccw_dmg  .~ d3
  & ccw_name .~ "force sword"

forceStave :: CCWeapon
forceStave = forceSword
  & ccw_strMod .~ Add 2
  & ccw_ap     .~ -1
  & ccw_name   .~ "force sword"

thunderHammer :: CCWeapon
thunderHammer = basic_ccw
  & ccw_strMod    .~ Times 2
  & ccw_ap        .~ -3
  & ccw_dmg       .~ return 3
  & ccw_name      .~ "thunder hammer"
  & makeUnwieldly
