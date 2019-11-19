module W40K.Data.ImperialKnights where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common

import qualified W40K.Data.Marines as Marines



-- MODIFIERS

ionFlareShield :: Model -> Model
ionFlareShield = (model_rng_inv .~ 4) . (model_cc_inv .~ 5)

ionBulwark :: Model -> Model
ionBulwark = (model_rng_inv %~ min 4) . (model_name <>~ " (bulwark)")

rotateIonShields :: Model -> Model
rotateIonShields = (model_inv %~ max 4 . subtract 1) . (model_name <>~ " (RIO)")

firstKnight :: Model -> Model
firstKnight = model_mods.mod_rrtohit <>~ RerollOnes

helmDominatus :: Model -> Model
helmDominatus = model_mods.mod_tohit +~ 1

headman'sMark :: EquippedModel -> EquippedModel
headman'sMark em = em
  & em_rw.mapped.as_weapon.w_dmg  %~ fmap (+1)
  & em_ccw.as_weapon.w_dmg %~ fmap (+1)

krastTradition :: Model -> Model
krastTradition = model_cc_mods.mod_rrtohit <>~ RerollFailed

controlledAggression :: EquippedModel -> EquippedModel
controlledAggression =
    em_ccw.as_weapon.w_hooks.hook_hit %~ addHook (MinUnmodifiedRoll 6) (HitHookExtraHits 1)

controlledAggressionChaos :: EquippedModel -> EquippedModel
controlledAggressionChaos =
    em_ccw.as_weapon.w_hooks.hook_hit %~ addHook (MinUnmodifiedRoll 6) (HitHookExtraHits 2)


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

cerastusModel :: Model
cerastusModel = questorisModel
  & model_wnd         .~ 27
  & model_name        .~ "cerastus model"

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
  & ccw_strMod   .~ Times 2
  & ccw_ap       .~ -4
  & ccw_dmg      .~ return 6
  & ccw_name     .~ "thunderstrike gauntlet"
  & makeUnwieldly

hekatonSiegeClaw :: CCWeapon
hekatonSiegeClaw = thunderstrikeGauntlet
  & ccw_name     .~ "hekaton siege claw"

atroposLascutter :: CCWeapon
atroposLascutter = basic_ccw
  & ccw_strMod   .~ ConstVal 14
  & ccw_ap       .~ -4
  & ccw_dmg      .~ return 6
  & ccw_weapon.w_mods.mod_rrtohit   .~ RerollFailed -- only against monsters, vehicles and buildings
  & ccw_weapon.w_mods.mod_rrtowound .~ RerollFailed -- only against monsters, vehicles and buildings
  & ccw_name     .~ "atropos lascutter"

ravager :: CCWeapon
ravager = basic_ccw
  & ccw_strMod   .~ Add 8
  & ccw_ap       .~ -4
  & ccw_dmg      .~ return 6
  & ccw_weapon.w_mods.mod_rrtohit .~ RerollOnes
  & ccw_name     .~ "ravager"

paragonGauntlet :: CCWeapon
paragonGauntlet = basic_ccw
  & ccw_strMod   .~ Times 2
  & ccw_ap       .~ -4
  & ccw_dmg      .~ return 8
  & ccw_name     .~ "paragon gauntlet"

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

stormspearRocketPod :: RngWeapon
stormspearRocketPod = krakMissile
  & rw_shots .~ return 3
  & rw_name  .~ "stormspear rocket pod"

radCleanser :: RngWeapon
radCleanser = flamer
  & rw_weapon.w_wounding .~ FixedWoundingAgainst [Infantry, Monster, Swarm, Cavalry, Biker, Beast, Battlesuit] 3
  & rw_dmg               .~ return 3
  & rw_name              .~ "rad-cleanser"

volkiteChieorovile :: RngWeapon
volkiteChieorovile = lascannon
  & rw_shots .~ return 5
  & rw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookExtraHits 1)
  & rw_name  .~ "volkite chieorovile"

gravitonCrusher :: RngWeapon
gravitonCrusher = lascannon
  & rw_shots .~ d3
  & rw_str   .~ 6
  & rw_ap    .~ -2
  & rw_dmg   .~ return 3 -- only for 3+ saves or better, but they are common
  & rw_name  .~ "graviton singularity cannon"

gravitonSingularityCannon :: RngWeapon
gravitonSingularityCannon = lascannon
  & rw_shots .~ return 4
  & rw_str   .~ 8
  & rw_dmg   .~ return 3
  & rw_name  .~ "graviton singularity cannon"

atroposLascutterShot :: RngWeapon
atroposLascutterShot = lascannon
  & rw_str   .~ 12
  & rw_ap    .~ -4
  & rw_dmg   .~ return 6
  & rw_name  .~ "atropos lascutter (shot)"

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

cerastus :: String -> CCWeapon -> [RngWeapon] -> EquippedModel
cerastus name ccw rws = basicEquippedModel cerastusModel
  & em_rw    .~ rws
  & em_ccw   .~ ccw
  & em_name  .~ "knight " ++ name ++ " w/" ++ ccw^.ccw_name

knightGallant :: CCWeapon -> EquippedModel
knightGallant ccw = questoris "gallant" ccw []
  & em_model.model_ws  .~ 2
  & em_model.model_att .~ 5

knightErrant :: [RngWeapon] -> CCWeapon -> EquippedModel
knightErrant rw ccw = questoris "errant" ccw (rw ++ [thermalCannon])

knightWarden :: [RngWeapon] -> CCWeapon -> EquippedModel
knightWarden rw ccw = questoris "warden" ccw (rw ++ [avengerGatlingCannon])

knightPaladin :: [RngWeapon] -> CCWeapon -> EquippedModel
knightPaladin rw ccw = questoris "paladin" ccw (rw ++ [rapidFireBattleCannon])

knightCrusader :: [RngWeapon] -> RngWeapon -> EquippedModel
knightCrusader rws armrw = stomping $ questoris "crusader" basic_ccw (rws ++ [armrw, avengerGatlingCannon])

knightStyrix :: [RngWeapon] -> EquippedModel
knightStyrix rw = questoris "styrix" hekatonSiegeClaw (volkiteChieorovile : gravitonCrusher : rw)
  & em_model %~ ionFlareShield

knightAtropos :: EquippedModel
knightAtropos = cerastus "atropos" atroposLascutter [atroposLascutterShot, gravitonSingularityCannon]

knightCastellan :: RngWeapon -> EquippedModel
knightCastellan plasmaArm = basicEquippedModel dominusModel
  & em_rw     .~ [volcanoLance] ++ two [twin meltagun] ++ [plasmaArm] ++ two [twin siegeBreakerCannon] ++ [shieldBreakerMissile]
  & em_name   .~ "knight castellan"
