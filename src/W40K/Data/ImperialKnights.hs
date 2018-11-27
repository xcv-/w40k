module W40K.Data.ImperialKnights where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common

import qualified W40K.Data.Marines as Marines



-- MODELS

armigerModel :: Model
armigerModel = Marines.dreadnought
  & model_wnd         .~ 12
  & model_rng_inv     .~ 5
  & model_ignoreHeavy .~ True  -- not really, but it only makes a difference with a single heavy stubber
  & model_ld          .~ 8
  & model_name        .~ "armiger knight"

questorisModel :: Model
questorisModel = armigerModel
  & model_str         .~ 8
  & model_tgh         .~ 8
  & model_wnd         .~ 24
  & model_ld          .~ 9
  & model_name        .~ "questoris knight"

dominusModel :: Model
dominusModel = questorisModel
  & model_wnd         .~ 28
  & model_name        .~ "dominus knight"


-- CC WEAPONS

chainCleaverCrushing :: CCWeapon
chainCleaverCrushing = basic_ccw
  & ccw_strMod   .~ Times 2
  & ccw_ap       .~ -3
  & ccw_dmg      .~ return 3
  & ccw_name     .~ "reaper chain-cleaver (crushing)"

chainCleaverSweeping :: CCWeapon
chainCleaverSweeping = basic_ccw
  & ccw_ap       .~ -2
  & ccw_name     .~ "reaper chain-cleaver (sweeping)"

titanicFeet :: CCWeapon
titanicFeet = basic_ccw
  & ccw_attBonus .~ Times 3
  & ccw_ap       .~ -2
  & ccw_dmg      .~ d3
  & ccw_name     .~ "titanic feet"

reaperChainsword :: CCWeapon
reaperChainsword = basic_ccw
  & ccw_strMod   .~ Add 6
  & ccw_ap       .~ -3
  & ccw_dmg      .~ return 6
  & ccw_name     .~ "reaper chainsword"

thunderstrikeGauntlet :: CCWeapon
thunderstrikeGauntlet = basic_ccw
  & ccw_strMod    .~ Times 2
  & ccw_ap        .~ -4
  & ccw_dmg       .~ return 6
  & ccw_name     .~ "thunderstrike gauntlet"
  & makeUnwieldly

-- RANGED WEAPONS

armigerAutocannon :: RngWeapon
armigerAutocannon = Marines.predatorAutocannon
  & rw_name .~ "helverin autocannon"

thermalSpear :: RngWeapon
thermalSpear = meltagun
  & rw_shots .~ d3
  & rw_name  .~ "thermal spear"

rapidFireBattleCannon :: RngWeapon
rapidFireBattleCannon = krakMissile
  & rw_shots  .~ twice d6
  & rw_dmg    .~ d3
  & rw_name   .~ "rapid-fire battle cannon"

avengerGatlingCannon :: RngWeapon
avengerGatlingCannon = assaultCannon
  & rw_shots .~ return 12
  & rw_ap    .~ -2
  & rw_dmg   .~ return 2
  & rw_name  .~ "avenger gatling cannon"

thermalCannon :: RngWeapon
thermalCannon = multimelta
  & rw_shots .~ d6
  & rw_str   .~ 9
  & rw_name  .~ "thermal cannon"

siegeBreakerCannon :: RngWeapon
siegeBreakerCannon = autocannon
  & rw_shots .~ d3
  & rw_dmg   .~ d3
  & rw_name  .~ "siegebreaker cannon"

shieldBreakerMissile :: RngWeapon
shieldBreakerMissile = lascannon
  & rw_str            .~ 10
  & rw_ap             .~ -4
  & rw_weapon.w_noinv .~ True
  & rw_name           .~ "shieldbreaker missile"

plasmaDecimator :: Bool -> RngWeapon
plasmaDecimator overcharged = plasmaCannon overcharged
  & rw_shots .~ twice d6
  & rw_name  .~ "plasma decimator"


volcanoLance :: RngWeapon
volcanoLance = lascannon
  & rw_shots .~ d6
  & rw_str   .~ 12
  & rw_ap    .~ -5
  & rw_dmg   .~ thrice d3
  & rw_name  .~ "volcano lance"
  -- & rw_mods.mod_rrtowound .~ RerollFailed


-- EQUIPPED MODELS

sweeping :: EquippedModel -> EquippedModel
sweeping em
  | em^.em_ccw == chainCleaverCrushing = em & em_ccw .~ chainCleaverSweeping
  | otherwise                          = em

armigerWarglaive :: EquippedModel
armigerWarglaive = basicEquippedModel armigerModel
  & em_rw     .~ [meltagun, thermalSpear]
  & em_ccw    .~ chainCleaverCrushing
  & em_name   .~ "armiger warglaive w/meltagun"

armigerHelverin :: EquippedModel
armigerHelverin = basicEquippedModel armigerModel
  & em_rw     .~ heavyStubber : two [armigerAutocannon]
  & em_name   .~ "armiger helverin"


stomping :: EquippedModel -> EquippedModel
stomping = (em_ccw .~ titanicFeet) . (em_name <>~ " (stomping)")

questoris :: String -> CCWeapon -> [RngWeapon] -> EquippedModel
questoris name ccw rws = basicEquippedModel questorisModel
  & em_rw    .~ rws
  & em_ccw   .~ ccw
  & em_name  .~ "knight " ++ name ++ " w/" ++ ccw^.ccw_name

knightErrant :: CCWeapon -> EquippedModel
knightErrant ccw = questoris "errant" ccw [thermalCannon, heavyStubber]

knightWarden :: CCWeapon -> EquippedModel
knightWarden ccw = questoris "warden" ccw [avengerGatlingCannon, heavyFlamer, heavyStubber]

knightPaladin :: CCWeapon -> EquippedModel
knightPaladin ccw = questoris "paladin" ccw [rapidFireBattleCannon, heavyStubber, heavyStubber]

knightCrusader :: [RngWeapon] -> EquippedModel
knightCrusader rw = stomping $ questoris "crusader" basic_ccw (rw ++ [avengerGatlingCannon, heavyFlamer, heavyStubber])

knightCastellan :: RngWeapon -> EquippedModel
knightCastellan plasmaArm = basicEquippedModel dominusModel
  & em_rw     .~ [volcanoLance] ++ two [twin meltagun] ++ [plasmaArm] ++ two [twin siegeBreakerCannon] ++ [shieldBreakerMissile]
  & em_name   .~ "knight castellan"
