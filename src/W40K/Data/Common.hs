{-# language RebindableSyntax #-}
module W40K.Data.Common where

import Prelude hiding (Functor(..), Monad(..))
import Control.Lens ((&), (^.), (.~), (%~))

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Core.Psychic

-- TOOLS AND MODIFIERS

rapidFiring :: [EquippedModel] -> [EquippedModel]
rapidFiring = map $ \em ->
    if em^.em_rw.rw_class == RapidFire then
      em & em_rw.rw_shots %~ fmap (*2)
    else
      em

closeEnough :: [EquippedModel] -> [EquippedModel]
closeEnough = (rapidFiring .) $ map $ \em ->
    if em^.em_rw.rw_melta then
      em & em_rw.rw_dmg %~ \dmg -> liftA2 max dmg dmg
    else
      em

moving :: [EquippedModel] -> [EquippedModel]
moving = map $ em_model.model_moved .~ True

twice :: (Ord a, Num a) => Prob a -> Prob a
twice p = sumProbs (replicate 2 p)

thrice :: (Ord a, Num a) => Prob a -> Prob a
thrice p = sumProbs (replicate 3 p)

twin :: RngWeapon -> RngWeapon
twin = rw_shots %~ twice

two :: a -> [a]
two a = [a,a]


-- MODELS

nosave :: Int
nosave = 9999

meq :: Model
meq = Model
  { _model_class            = Infantry
  , _model_ws               = 3
  , _model_bs               = 3
  , _model_str              = 4
  , _model_tgh              = 4
  , _model_att              = 1
  , _model_wnd              = 1
  , _model_ld               = 7
  , _model_save             = 3
  , _model_cc_inv           = nosave
  , _model_rng_inv          = nosave
  , _model_moved            = False
  , _model_cc_mods          = noMods
  , _model_rng_mods         = noMods
  , _model_quantumShielding = False
  , _model_machineSpirit    = False
  , _model_fnp              = 7
  , _model_name             = "MEQ"
  }

teq :: Model
teq = meq
  & model_att  .~ 2
  & model_wnd  .~ 2
  & model_ld   .~ 8
  & model_save .~ 2
  & model_inv  .~ 5
  & model_name .~ "TEQ"

rhino :: Model
rhino = meq
  & model_class .~ Vehicle
  & model_ws    .~  6
  & model_str   .~  6
  & model_tgh   .~  7
  & model_att   .~  3
  & model_wnd   .~ 10
  & model_ld    .~  8
  & model_save  .~  3
  & model_name  .~ "rhino"


-- PSYCHIC

smite :: PsychicPower
smite = PsychicPower
    { _power_castingValue = 5
    , _power_inflictMortalWounds = \_ _ v -> if v > 10 then d6 else d3
    }


-- RANGED WEAPONS

bolter :: RngWeapon
bolter = RngWeapon
  { _rw_shots   = return 1
  , _rw_str     = 4
  , _rw_class   = RapidFire
  , _rw_autohit = False
  , _rw_melta   = False
  , _rw_weapon  = basicWeapon "boltgun"
  }

boltPistol :: RngWeapon
boltPistol = bolter
  & rw_class .~ Pistol
  & rw_name  .~ "bolt pistol"

stormBolter :: RngWeapon
stormBolter = bolter
  & rw_shots .~ return 2
  & rw_name  .~ "storm bolter"

hurricaneBolter :: RngWeapon
hurricaneBolter = bolter
  & rw_shots .~ return 6
  & rw_name  .~ "hurricane bolter"

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

lascannon :: RngWeapon
lascannon = RngWeapon
  { _rw_shots   = return 1
  , _rw_str     = 9
  , _rw_class   = Heavy
  , _rw_autohit = False
  , _rw_melta   = False
  , _rw_weapon  = basicWeapon "lascannon"
    & w_ap  .~ -3
    & w_dmg .~ d6
  }

heavyPlasmaCannon :: RngWeapon
heavyPlasmaCannon = bolter
  & rw_shots .~ d3
  & rw_str   .~ 7
  & rw_ap    .~ -3
  & rw_dmg   .~ return 1
  & rw_name  .~ "heavy plasma cannon"

heavyPlasmaCannonSupercharge :: RngWeapon
heavyPlasmaCannonSupercharge = heavyPlasmaCannon
  & rw_str   .~ 8
  & rw_ap    .~ -3
  & rw_dmg   .~ return 2
  & rw_name  .~ "heavy plasma cannon (superchage)"

multimelta :: RngWeapon
multimelta = lascannon
  & rw_str   .~ 8
  & rw_ap    .~ -4
  & rw_name  .~ "multi-melta"
  & rw_melta .~ True

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

thunderHammer :: CCWeapon
thunderHammer = basic_ccw
  & ccw_strMod    .~ Times 2
  & ccw_ap        .~ -3
  & ccw_dmg       .~ return 3
  & ccw_unwieldly .~ True
  & ccw_name      .~ "thunder hammer"
