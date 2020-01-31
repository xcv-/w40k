module W40K.Data.Marines where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common


-- STRATAGEMS

transhumanPhysiology :: Model -> Model
transhumanPhysiology = model_unmodifiedMinWound %~ max 4


-- AURAS

captainAura :: Aura
captainAura = noAura & aura_any.mod_rrtohit .~ RerollOnes

liutenantAura :: Aura
liutenantAura = noAura & aura_any.mod_rrtowound .~ RerollOnes

guillimanAura :: Aura
guillimanAura = noAura
  & aura_any.mod_rrtohit   .~ RerollAll
  & aura_any.mod_rrtowound .~ RerollOnes

telionAbility :: Model -> Model
telionAbility = model_mods.mod_tohit +~ 1

strafingRun :: Model -> Model
strafingRun = model_mods.mod_tohit +~ 1

interceptorJet :: Model -> Model
interceptorJet = model_mods.mod_tohit +~ 1

fleshIsWeak :: Model -> Model
fleshIsWeak m = m
  & model_fnp  %~  min 6
  & model_name <>~ " (IH)"


-- MODELS

scout :: Model
scout = meq
  & model_save .~ 4
  & model_name .~ "scout"

captain :: Model
captain = meq
  & model_ws   .~ 2
  & model_bs   .~ 2
  & model_wnd  .~ 5
  & model_att  .~ 4
  & model_ld   .~ 9
  & model_name .~ "captain"

scoutSargeant :: Model
scoutSargeant = scout
  & model_att  .~ 2
  & model_ld   .~ 8
  & model_name .~ "scout sargeant"

guilliman :: Model
guilliman = meq
  & model_class .~ Monster
  & model_ws    .~ 2
  & model_bs    .~ 2
  & model_str   .~ 6
  & model_tgh   .~ 6
  & model_wnd   .~ 9
  & model_att   .~ 6
  & model_ld    .~ 10
  & model_save  .~ 2
  & model_inv   .~ 3
  & model_name  .~ "rowboat girlyman"

primaris :: Model
primaris = meq
  & model_att  .~ 2
  & model_wnd  .~ 2
  & model_name .~ "primaris marine"

gravis :: Model
gravis = meq
  & model_tgh  .~ 5
  & model_wnd  .~ 2
  & model_name .~ "gravis armor marine"

primarisAncient :: Model
primarisAncient = primaris
  & model_att  .~ 4
  & model_wnd  .~ 5
  & model_name .~ "primaris ancient"

dreadnought :: Model
dreadnought = meq
  & model_class .~ Vehicle
  & model_str   .~ 6
  & model_tgh   .~ 7
  & model_wnd   .~ 8
  & model_att   .~ 4
  & model_ld    .~ 8
  & model_name  .~ "dreadnought"

venDreadnought :: Model
venDreadnought = dreadnought
  & model_ws   .~ 2
  & model_bs   .~ 2
  & model_fnp  .~ 6
  & model_name .~ "venerable dreadnought"

redemptorDreadnought :: Model
redemptorDreadnought = dreadnought
  & model_att  .~  5
  & model_wnd  .~ 13
  & model_str  .~  6
  & model_tgh  .~  7
  & model_name .~ "redemptor dreadnought"

leviathanDreadnought :: Model
leviathanDreadnought = venDreadnought
  & model_tgh  .~ 8
  & model_wnd  .~ 14
  & model_save .~ 2
  & model_inv  .~ 4
  & model_fnp  .~ 7
  & model_name .~ "leviathan dreadnought"

razorback :: Model
razorback = rhino
  & model_name .~ "razorback"

predator :: Model
predator = rhino
  & model_wnd  .~ 11
  & model_name .~ "predator"

stormtalon :: Model
stormtalon = rhino
  & model_str                  .~ 6
  & model_tgh                  .~ 6
  & model_wnd                  .~ 10
  & model_rng_mods.mod_tobehit .~ -1
  & model_name                 .~ "stormtalon"

stormhawk :: Model
stormhawk = stormtalon
  & model_tgh                  .~ 7
  & model_rng_mods.mod_rrarmor .~ RerollOnes
  & model_name                 .~ "stormhawk"

stormraven :: Model
stormraven = stormtalon
  & model_str                  .~ 8
  & model_tgh                  .~ 7
  & model_wnd                  .~ 14
  & model_ignoreHeavy          .~ True
  & model_name                 .~ "stormraven"

landraider :: Model
landraider = rhino
  & model_str         .~ 8
  & model_tgh         .~ 8
  & model_wnd         .~ 16
  & model_att         .~ 6
  & model_ld          .~ 9
  & model_save        .~ 2
  & model_ignoreHeavy .~ True
  & model_name        .~ "landraider"

repulsor :: Model
repulsor = landraider
  & model_save .~  3
  & model_name .~ "repulsor"


-- RANGED WEAPONS

sniperRifle :: RngWeapon
sniperRifle = bolter
  & rw_class                     .~ Heavy
  & rw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookMortalWounds (return 1))
  & rw_name                      .~ "sniper rifle"

quietusRifle :: RngWeapon
quietusRifle = sniperRifle
  & rw_shots                     .~ return 2
  & rw_ap                        .~ -1
  & rw_dmg                       .~ d3
  & rw_weapon.w_hooks.hook_wound .~ []
  & rw_name                      .~ "quietus rifle"

hunterkiller :: RngWeapon
hunterkiller = krakMissile
  & rw_name .~ "hunter-killer missile"

typhoonKrak :: RngWeapon
typhoonKrak = twin krakMissile
  & rw_name .~ "typhoon krak missile"

stormstrike :: RngWeapon
stormstrike = lascannon
  & rw_str  .~ 8
  & rw_ap   .~ -3
  & rw_dmg  .~ return 3
  & rw_name .~ "stormstrike"

predatorAutocannon :: RngWeapon
predatorAutocannon = lascannon
  & rw_shots   .~ twice d3
  & rw_str     .~ 7
  & rw_ap      .~ -1
  & rw_dmg     .~ return 3
  & rw_name    .~ "predator autocannon"

lastalon :: RngWeapon
lastalon = twin lascannon
  & rw_name    .~ "las-talon"

helfrostDestructorDispersed :: RngWeapon
helfrostDestructorDispersed = lascannon
  & rw_shots .~ thrice d3
  & rw_str   .~ 6
  & rw_ap    .~ -2
  & rw_dmg   .~ return 2
  & rw_name  .~ "helfrost destructor (dispersed)"

helfrostDestructorFocused :: RngWeapon
helfrostDestructorFocused = lascannon
  & rw_shots .~ return 3
  & rw_str   .~ 8
  & rw_ap    .~ -4
  & rw_dmg   .~ d6
  & rw_name  .~ "helfrost destructor (focused)"


-- EQUIPPED MODELS

bolterTactical :: EquippedModel
bolterTactical = basicEquippedModel meq
  & em_rw   .~ [bolter]
  & em_name .~ "tactical marine"

intercessor :: EquippedModel
intercessor = basicEquippedModel primaris
  & em_rw    .~ [bolter & rw_ap .~ -1]
  & em_name  .~ "primaris intercessor"

telion :: EquippedModel
telion = basicEquippedModel scoutSargeant
  & em_model.model_bs  .~ 2
  & em_model.model_wnd .~ 4
  & em_rw              .~ [quietusRifle]
  & em_name            .~ "sargeant telion"

razorbackWith :: [RngWeapon] -> EquippedModel
razorbackWith rw = basicEquippedModel razorback
  & em_rw    .~ rw
  & em_name  .~ "razorback w/ " ++ weaponNames rw

predatorWith :: [RngWeapon] -> EquippedModel
predatorWith rw = basicEquippedModel predator
  & em_rw    .~ rw
  & em_name  .~ "predator w/ " ++ weaponNames rw

landraiderWith :: [RngWeapon] -> EquippedModel
landraiderWith rw = basicEquippedModel landraider
  & em_rw    .~ rw
  & em_name  .~ "landraider w/ " ++ weaponNames rw

stormtalonWith :: [RngWeapon] -> EquippedModel
stormtalonWith rw = basicEquippedModel stormtalon
  & em_rw    .~ rw
  & em_name  .~ "stormtalon w/ " ++ weaponNames rw

stormhawkWith :: [RngWeapon] -> EquippedModel
stormhawkWith rw = basicEquippedModel stormhawk
  & em_rw    .~ rw
  & em_name  .~ "stormhawk w/ " ++ weaponNames rw

stormravenWith :: [RngWeapon] -> EquippedModel
stormravenWith rw = basicEquippedModel stormraven
  & em_rw    .~ rw
  & em_name  .~ "stormraven w/ " ++ weaponNames rw

stormfangWith :: [RngWeapon] -> EquippedModel
stormfangWith rw = rename (stormravenWith rw)
  where
    rename = em_name .~ "stormfang gunship w/ " ++ weaponNames rw

stormfangWithAvWith :: RngWeapon -> EquippedModel
stormfangWithAvWith mainCannon =
    stormfangWith ([mainCannon] ++ two [lascannon] ++ two [twin multimelta])
    & em_name .~ "stormfang w/ LC+MM+" ++ (mainCannon^.rw_name)

-- [EQUIPPED MODELS]

sniperSquad :: Int -> [EquippedModel]
sniperSquad n =
    (basicEquippedModel scoutSargeant & em_rw .~ [sniperRifle])
    : replicate (n-1) (basicEquippedModel scout & em_rw .~ [sniperRifle])

laserRazorback :: EquippedModel
laserRazorback = razorbackWith [twin lascannon]

meltaStormraven :: EquippedModel
meltaStormraven =
    stormravenWith ([twin multimelta] ++ two [stormstrike])
    & em_name .~ "stormraven w/ MM+SS"

fullAvVenDreadnought :: EquippedModel
fullAvVenDreadnought = basicEquippedModel venDreadnought
    & em_rw   .~ [twin lascannon, krakMissile]
    & em_name .~ "ven dreadnought w/ LC+ML"

fullAvStormraven :: EquippedModel
fullAvStormraven =
    stormravenWith ([twin multimelta] ++ two [lascannon] ++ two [stormstrike])
    & em_name .~ "stormraven w/ MM+LC+SS"

assaultCannonStormraven :: EquippedModel
assaultCannonStormraven =
    stormravenWith ([twin assaultCannon] ++ two [hurricaneBolter])
    & em_name .~ "stormraven w/ AC+HB"

godhammerLandraider :: EquippedModel
godhammerLandraider =
    landraiderWith (two [twin lascannon])
    & em_name .~ "landraider w/ LC"

fullAvGodhammerLandraider :: EquippedModel
fullAvGodhammerLandraider =
    landraiderWith (two [twin lascannon] ++ [multimelta] ++ [hunterkiller])
    & em_name .~ "landraider w/ LC+MM+HKM"

landraiderCrusader :: EquippedModel
landraiderCrusader =
    landraiderWith (two [hurricaneBolter] ++ [twin assaultCannon])
    & em_name .~ "landraider crusader"

