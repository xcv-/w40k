module W40K.Data.Marines where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common

-- AURAS

captainAura :: Aura
captainAura = noAura & aura_any.mod_rrtohit .~ RerollOnes

liutenantAura :: Aura
liutenantAura = noAura & aura_any.mod_rrtowound .~ RerollOnes

guillimanAura :: Aura
guillimanAura = noAura & aura_any.mod_rrtohit   .~ RerollFailed
                       & aura_any.mod_rrtowound .~ RerollFailed

telionAbility :: Modifier
telionAbility = em_model.model_mods.mod_tohit +~ 1

-- MODELS

scout :: Model
scout = meq
  & model_save .~ 4
  & model_name .~ "scout"

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

razorback :: Model
razorback = rhino
  & model_name .~ "razorback"

predator :: Model
predator = rhino
  & model_wnd  .~ 11
  & model_name .~ "predator"

repulsor :: Model
repulsor = landraider
  & model_save .~  3
  & model_name .~ "repulsor"

stormraven :: Model
stormraven = rhino
  & model_str                  .~ 8
  & model_tgh                  .~ 7
  & model_wnd                  .~ 14
  & model_rng_mods.mod_tobehit .~ -1
  & model_machineSpirit        .~ True
  & model_name                 .~ "stormraven"

landraider :: Model
landraider = rhino
  & model_wnd           .~ 16
  & model_str           .~  8
  & model_tgh           .~  8
  & model_att           .~  6
  & model_ld            .~ 9
  & model_save          .~  2
  & model_machineSpirit .~ True
  & model_name          .~ "landraider"

knightPaladin :: Model
knightPaladin = meq
  & model_class         .~ Vehicle
  & model_str           .~ 8
  & model_tgh           .~ 8
  & model_wnd           .~ 24
  & model_att           .~ 4
  & model_ld            .~ 9
  & model_rng_inv       .~ 5
  & model_machineSpirit .~ True
  & model_name          .~ "knight paladin"


-- CC WEAPONS

titanicFeet :: CCWeapon
titanicFeet = basic_ccw
  & ccw_attBonus .~ Times 3
  & ccw_ap       .~ -2
  & ccw_dmg      .~ d3

-- RANGED WEAPONS

sniperRifle :: RngWeapon
sniperRifle = bolter
  & rw_class                     .~ Heavy
  & rw_weapon.w_hooks.hook_wound .~ Just (RollHook 6 (WoundHookMortalWounds (return 1)))
  & rw_name                      .~ "sniper rifle"

quietusRifle :: RngWeapon
quietusRifle = sniperRifle
  & rw_shots                     .~ return 2
  & rw_ap                        .~ -1
  & rw_dmg                       .~ d3
  & rw_weapon.w_hooks.hook_wound .~ Nothing
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

rapidFireBattleCannon :: RngWeapon
rapidFireBattleCannon = predatorAutocannon
  & rw_shots  .~ twice d6
  & rw_str    .~ 8
  & rw_ap     .~ -2
  & rw_dmg    .~ d3

-- EQUIPPED MODELS

bolterTactical :: EquippedModel
bolterTactical = basicEquippedModel meq
  & em_rw   .~ bolter
  & em_name .~ "tactical marine"

intercessor :: EquippedModel
intercessor = basicEquippedModel primaris
  & em_rw    .~ (bolter & rw_ap .~ -1)
  & em_name  .~ "primaris intercessor"

telion :: EquippedModel
telion = basicEquippedModel scoutSargeant
  & em_model.model_bs  .~ 2
  & em_model.model_wnd .~ 4
  & em_rw              .~ quietusRifle
  & em_name            .~ "sargeant telion"

razorback's :: RngWeapon -> EquippedModel
razorback's rw = basicEquippedModel razorback
  & em_rw    .~ rw
  & em_name  .~ "razorback's " ++ rw^.rw_name

predator's :: RngWeapon -> EquippedModel
predator's rw = basicEquippedModel predator
  & em_rw    .~ rw
  & em_name  .~ "predator's " ++ rw^.rw_name

landraider's :: RngWeapon -> EquippedModel
landraider's rw = basicEquippedModel landraider
  & em_rw    .~ rw
  & em_name  .~ "landraider's " ++ rw^.rw_name

stormraven's :: RngWeapon -> EquippedModel
stormraven's rw = basicEquippedModel stormraven
  & em_rw    .~ rw
  & em_name  .~ "stormraven's " ++ rw^.rw_name

stormfang's :: RngWeapon -> EquippedModel
stormfang's rw = rename (stormraven's rw)
  where
    rename = em_name .~ "stormfang gunship's " ++ rw^.rw_name

eqStormfang :: RngWeapon -> [EquippedModel]
eqStormfang cannonMode =
    stormfang's cannonMode
      : two [stormfang's lascannon]
      ++ two [stormfang's (twin multimelta)]

knightPaladin's :: CCWeapon -> RngWeapon -> EquippedModel
knightPaladin's ccw rw = basicEquippedModel knightPaladin
  & em_rw    .~ rw
  & em_ccw   .~ ccw
  & em_name  .~ "knight paladin's " ++ ccw^.ccw_name ++ "/" ++ rw^.rw_name


-- [EQUIPPED MODELS]

sniperSquad :: Int -> [EquippedModel]
sniperSquad n =
    (basicEquippedModel scoutSargeant & em_rw .~ sniperRifle)
    : replicate (n-1) (basicEquippedModel scout & em_rw .~ sniperRifle)

meltaStormraven :: [EquippedModel]
meltaStormraven =
    stormraven's (twin multimelta)
    : two [stormraven's stormstrike]

fullAvStormraven :: [EquippedModel]
fullAvStormraven =
    stormraven's (twin multimelta)
    : stormraven's (twin lascannon)
    : two [stormraven's stormstrike]

assaultCannonStormraven :: [EquippedModel]
assaultCannonStormraven =
    stormraven's (twin assaultCannon)
    : two [stormraven's hurricaneBolter]

godhammerLandraider :: [EquippedModel]
godhammerLandraider =
    two [landraider's (twin lascannon)]

fullAvGodhammerLandraider :: [EquippedModel]
fullAvGodhammerLandraider =
    landraider's hunterkiller
    : landraider's multimelta
    : two [landraider's (twin lascannon)]
