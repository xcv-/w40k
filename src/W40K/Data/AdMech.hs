module W40K.Data.AdMech where

import Prelude hiding (Functor(..), Monad(..))
import Data.List (isInfixOf)

import Control.Lens

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Data.Common


-- AURAS

dominusAura :: Aura
dominusAura = noAura & aura_rng.mod_rrtohit .~ RerollOnes

primeHermeticonAura :: Aura
primeHermeticonAura = noAura & aura_cc.mod_rrtohit .~ RerollFailed

cawlAura :: Aura
cawlAura = noAura & aura_rng.mod_rrtohit .~ RerollAll

nearOnager :: Modifier
nearOnager = em_model.model_mods.mod_rrinv .~ RerollOnes


-- CANTICLES

chantOfTheRemorselessFist :: Modifier
chantOfTheRemorselessFist = em_model.model_cc_mods.mod_rrtohit .~ RerollOnes

invocationOfMachineMight :: Modifier
invocationOfMachineMight = em_model.model_str +~ 1

benedictionOfTheOmnissiah :: Modifier
benedictionOfTheOmnissiah = em_model.model_rng_mods.mod_rrtohit .~ RerollOnes


-- DOGMAS

ryzaDogma :: Modifier
ryzaDogma = em_model.model_cc_mods.mod_rrtowound .~ RerollOnes


-- STRATAGEMS & ABILITIES

dataTether :: Modifier
dataTether = em_model.model_name <>~ " (data-tether)"

protectorDoctrinaImperative :: Modifier
protectorDoctrinaImperative em =
    if "data-tether" `isInfixOf` name || "d-t" `isInfixOf` name then
      em & em_model.model_rng_mods.mod_tohit +~ 2
    else
      em & em_model.model_rng_mods.mod_tohit +~ 1
  where
    name = em^.em_model.model_name

conquerorDoctrinaImperative :: Modifier
conquerorDoctrinaImperative em =
    if "data-tether" `isInfixOf` name || "d-t" `isInfixOf` name then
      em & em_model.model_cc_mods.mod_tohit +~ 2
    else
      em & em_model.model_cc_mods.mod_tohit +~ 1
  where
    name = em^.em_model.model_name

neosphericMindlock :: Modifier
neosphericMindlock = em_model.model_rng_mods.mod_tohit +~ 1

eliminationVolley :: Modifier
eliminationVolley = em_model.model_rng_mods.mod_tohit +~ 1

plasmaSpecialists :: Modifier
plasmaSpecialists =
    em_rw.mapped.rw_weapon %~ \w ->
      if "plasma" `isInfixOf` (w^.w_name) then
        w & w_dmg              %~ fmap (+1)
          & w_mods.mod_towound +~ 1
      else
        w

wrathOfMars :: Modifier
wrathOfMars =
    em_rw.mapped.rw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookMortalWounds (return 1))


data Protocol = Aegis | Protector | Conqueror
  deriving (Eq, Ord, Enum, Bounded, Show)

withProtocol :: Protocol -> Modifier
withProtocol proto em =
  if "kastelan" `isInfixOf` (em^.em_model.model_name) then
    case proto of
      Aegis     -> em & em_model.model_mods.mod_tosave +~ 1
      Conqueror -> em & em_model.model_att  *~ 2
      Protector -> em & em_rw.mapped.rw_shots %~ fmap (*2)
  else
    em

radSaturation :: Model -> Model
radSaturation = (model_tgh -~ 1) . (model_name <>~ " (rad-saturation)")

eyeOfXiLexum :: Modifier
eyeOfXiLexum = em_model.model_mods.mod_rrtowound .~ RerollOnes


-- MODELS

rangerModel :: Model
rangerModel = geq
  & model_bs   .~ 3
  & model_ld   .~ 6
  & model_save .~ 4
  & model_inv  .~ 6
  & model_name .~ "skitarii ranger"

vanguardModel :: Model
vanguardModel = rangerModel
  & model_name .~ "skitarii vanguard"

alpha :: Model -> Model
alpha = (model_att +~ 1) . (model_ld +~ 1) . (model_name <>~ " alpha")

kataphronDestroyerModel :: Model
kataphronDestroyerModel = meq
  & model_ws          .~ 4
  & model_bs          .~ 4
  & model_str         .~ 5
  & model_tgh         .~ 5
  & model_wnd         .~ 3
  & model_att         .~ 2
  & model_ld          .~ 7
  & model_save        .~ 4
  & model_inv         .~ 6
  & model_ignoreHeavy .~ True
  & model_name        .~ "kataphron destroyer"

kataphronBreacherModel :: Model
kataphronBreacherModel = kataphronDestroyerModel
  & model_att  .~ 3
  & model_save .~ 3
  & model_name .~ "kataphron breacher"

ruststalkerModel :: Model
ruststalkerModel = rangerModel
  & model_ws   .~ 3
  & model_str  .~ 4
  & model_wnd  .~ 2
  & model_att  .~ 2
  & model_name .~ "sicarian ruststalker"

ruststalkerPrincepsModel :: Model
ruststalkerPrincepsModel = ruststalkerModel
  & model_att  +~ 1
  & model_ld   +~ 1
  & model_name .~ "ruststalker princeps"

infiltratorModel :: Model
infiltratorModel = ruststalkerModel
  & model_ws  .~ 3
  & model_str .~ 4
  & model_wnd .~ 2
  & model_att .~ 2
  & model_name .~ "sicarian infiltrator"

infiltratorPrincepsModel :: Model
infiltratorPrincepsModel = infiltratorModel
  & model_att  +~ 1
  & model_ld   +~ 1
  & model_name .~ "infiltrator princeps"

ironstriderModel :: Model
ironstriderModel = rhino
  & model_ws      .~ 3
  & model_bs      .~ 3
  & model_str     .~ 5
  & model_tgh     .~ 6
  & model_att     .~ 3
  & model_wnd     .~ 6
  & model_ld      .~ 8
  & model_save    .~ 4
  & model_inv     .~ 6
  & model_name    .~ "ironstrider ballistarius (d-t)"

dragoonModel :: Model
dragoonModel = ironstriderModel
  & model_rng_mods.mod_tobehit .~ -1
  & model_name    .~ "sydonian dragoon (d-t)"

kastelanModel :: Model
kastelanModel = rhino
  & model_ws      .~ 4
  & model_bs      .~ 4
  & model_att     .~ 3
  & model_wnd     .~ 6
  & model_ld      .~ 10
  & model_save    .~ 3
  & model_rng_inv .~ 5
  & model_name    .~ "kastelan robot"

onagerModel :: Model
onagerModel = rhino
  & model_ws          .~ 5
  & model_wnd         .~ 11
  & model_inv         .~ 5
  & model_ignoreHeavy .~ True
  & model_name        .~ "onager dunecrawler (d-t)"

-- RANGED WEAPONS

galvanicRifleAp0 :: RngWeapon
galvanicRifleAp0 = bolter
  & rw_name     .~ "galvanic rifle (AP 0)"

galvanicRifleAp1 :: RngWeapon
galvanicRifleAp1 = galvanicRifleAp0
  & rw_ap       .~ -1
  & rw_name     .~ "galvanic rifle (AP -1)"

galvanicRifle :: RngWeapon
galvanicRifle = galvanicRifleAp0
  & rw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookModWeapon (galvanicRifleAp1^.rw_weapon))
  & rw_name                      .~ "galvanic rifle"

radiumCarbineDmg1 :: RngWeapon
radiumCarbineDmg1 = bolter
  & rw_shots    .~ return 3
  & rw_str      .~ 3
  & rw_class    .~ Assault
  & rw_name     .~ "radium carbine (damage 1)"

radiumCarbineDmg2 :: RngWeapon
radiumCarbineDmg2 = radiumCarbineDmg1
  & rw_dmg      .~ return 2
  & rw_name     .~ "radium carbine (damage 2)"

radiumCarbine :: RngWeapon
radiumCarbine = radiumCarbineDmg1
  & rw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookModWeapon (radiumCarbineDmg2^.rw_weapon))
  & rw_name     .~ "radium carbine"

arcRifle :: ModelClass -> RngWeapon
arcRifle mc = bolter
  & rw_shots    .~ return 1
  & rw_str      .~ 6
  & rw_ap       .~ -1
  & rw_dmg      .~ (if mc == Vehicle then d3 else return 1)
  & rw_name     .~ "arc rifle"

plasmaCaliver :: RngWeapon
plasmaCaliver = radiumCarbineDmg1
  & rw_shots    .~ return 2
  & rw_str      .~ 7
  & rw_ap       .~ -3
  & rw_name     .~ "plasma caliver"

plasmaCaliverOvercharge :: RngWeapon
plasmaCaliverOvercharge = plasmaCaliver
  & rw_str      .~ 8
  & rw_dmg      .~ return 2
  & rw_name     .~ "plasma caliver (overcharge)"

transuranicArquebus :: RngWeapon
transuranicArquebus = lascannon
  & rw_str      .~ 7
  & rw_ap       .~ -2
  & rw_dmg      .~ d3
  & rw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookMortalWounds (return 1))
  & rw_name     .~ "transuranic arquebus"

plasmaCulverin :: RngWeapon
plasmaCulverin = plasmaCaliver
  & rw_class   .~ Heavy
  & rw_shots   .~ d6
  & rw_name    .~ "plasma culverin"

plasmaCulverinOvercharge :: RngWeapon
plasmaCulverinOvercharge = plasmaCaliverOvercharge
  & rw_class   .~ Heavy
  & rw_shots   .~ d6
  & rw_name    .~ "plasma culverin (overcharge)"

heavyGravCannon_d3 :: RngWeapon
heavyGravCannon_d3 = heavyBolter
  & rw_shots    .~ return 5
  & rw_str      .~ 5
  & rw_ap       .~ -3
  & rw_dmg      .~ d3
  & rw_name     .~ "heavy grav-cannon (D3 dmg)"

heavyArcRifle :: ModelClass -> RngWeapon
heavyArcRifle mc = heavyBolter
  & rw_shots    .~ return 2
  & rw_str      .~ 6
  & rw_ap       .~ -2
  & rw_dmg      .~ (if mc == Vehicle then d6 else d3)
  & rw_name     .~ "heavy arc rifle"

torsionCannon :: RngWeapon
torsionCannon = lascannon
  & rw_str      .~ 8
  & rw_ap       .~ -4
  & rw_name     .~ "torsion cannon"

flechetteBlaster :: RngWeapon
flechetteBlaster = boltPistol
  & rw_shots    .~ return 5
  & rw_str      .~ 3
  & rw_name     .~ "flechette blaster"

stubcarbine :: RngWeapon
stubcarbine = boltPistol
  & rw_shots    .~ return 3
  & rw_name     .~ "stubcarbine"

phosphorBlaster :: RngWeapon
phosphorBlaster = bolter
  & rw_str      .~ 5
  & rw_ap       .~ -1
  & rw_name     .~ "phosphor blaster"

heavyPhosphorBlaster :: RngWeapon
heavyPhosphorBlaster = heavyBolter
  & rw_str      .~ 6
  & rw_ap       .~ -2
  & rw_name     .~ "heavy phosphor blaster"

eradicationBeamerShort :: RngWeapon
eradicationBeamerShort = lascannon
  & rw_shots    .~ d3
  & rw_str      .~ 8
  & rw_ap       .~ -4
  & rw_dmg      .~ d6
  & rw_name     .~ "eradication beamer (short)"

eradicationBeamerLong :: RngWeapon
eradicationBeamerLong = lascannon
  & rw_shots    .~ d6
  & rw_str      .~ 8
  & rw_ap       .~ -2
  & rw_dmg      .~ d3
  & rw_name     .~ "eradication beamer (long)"

gatlingRocketLauncher :: RngWeapon
gatlingRocketLauncher = heavyPhosphorBlaster
  & rw_shots    .~ return 5
  & rw_name     .~ "gatling rocket launcher"

icarusAutocannon :: RngWeapon
icarusAutocannon = twin autocannon
  & rw_name     .~ "icarus autocannon"

daedalusMissile :: RngWeapon
daedalusMissile = lascannon
  & rw_str      .~ 7
  & rw_name     .~ "daedalus missile"

icarusArray :: Bool -> [RngWeapon]
icarusArray fly =
    map (rw_mods.mod_tohit +~ if fly then 1 else -1)
        [gatlingRocketLauncher, icarusAutocannon, daedalusMissile]

neutronLaser :: RngWeapon
neutronLaser = lascannon
  & rw_shots     .~ d3
  & rw_str       .~ 10
  & rw_ap        .~ -4
  & rw_dmg       .~ fmap (max 3) d6
  & rw_name      .~ "neutron laser"


-- CC WEAPONS

arcClaw :: ModelClass -> CCWeapon
arcClaw mc = basic_ccw
  & ccw_strMod .~ Add 1
  & ccw_ap     .~ -1
  & ccw_dmg    .~ (if mc == Vehicle then d3 else return 1)
  & ccw_name   .~ "arc claw"

hydraulicClaw :: CCWeapon
hydraulicClaw = basic_ccw
  & ccw_strMod .~ Times 2
  & ccw_ap     .~ -1
  & ccw_dmg    .~ d3
  & ccw_name   .~ "hydraulic claw"
  & makeUnwieldly

transonicRazor :: CCWeapon
transonicRazor = basic_ccw
  & ccw_name   .~ "transonic razor"
  & ccw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookMortalDamage (return 1))

transonicBlades :: CCWeapon
transonicBlades = basic_ccw
  & ccw_strMod .~ Add 1
  & ccw_name   .~ "transonic blades"
  & ccw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookMortalDamage (return 1))

chordClaw :: CCWeapon
chordClaw = basic_ccw
  & ccw_dmg    .~ d3
  & ccw_name   .~ "chord claw"
  & ccw_weapon.w_hooks.hook_wound %~ addHook (MinModifiedRoll 6) (WoundHookMortalDamage d3)

taserGoad :: CCWeapon
taserGoad = basic_ccw
  & ccw_strMod .~ Add 2
  & ccw_name   .~ "taser goad"
  & ccw_weapon.w_hooks.hook_hit %~ addHook (MinModifiedRoll 6) (HitHookExtraHits 2)

taserLance :: CCWeapon
taserLance = basic_ccw
  & ccw_strMod .~ Add 3
  & ccw_ap     .~ -1
  & ccw_dmg    .~ return 2
  & ccw_name   .~ "taser lance"
  & ccw_weapon.w_hooks.hook_hit %~ addHook (MinModifiedRoll 6) (HitHookExtraHits 2)


-- EQUIPPED MODELS

rangerWith :: RngWeapon -> EquippedModel
rangerWith rw = basicEquippedModel rangerModel
  & em_rw    .~ [rw]
  & em_name  .~ "ranger w/ " ++ (rw^.rw_name)

vanguardWith :: RngWeapon -> EquippedModel
vanguardWith rw = basicEquippedModel vanguardModel
  & em_rw    .~ [rw]
  & em_name  .~ "vanguard w/ " ++ (rw^.rw_name)

plasmaKataphron :: EquippedModel
plasmaKataphron = basicEquippedModel kataphronDestroyerModel
  & em_rw    .~ [phosphorBlaster, plasmaCulverin]
  & em_name  .~ "plasma kataphron destroyer"

plasmaKataphronOvercharge :: EquippedModel
plasmaKataphronOvercharge = basicEquippedModel kataphronDestroyerModel
  & em_rw    .~ [phosphorBlaster, plasmaCulverinOvercharge]
  & em_name  .~ "plasma kataphron destroyer (overcharge)"

gravKataphron_d3 :: EquippedModel
gravKataphron_d3 = basicEquippedModel kataphronDestroyerModel
  & em_rw    .~ [phosphorBlaster, heavyGravCannon_d3]
  & em_name  .~ "grav kataphron destroyer (D3 dmg)"

arcKataphron :: ModelClass -> EquippedModel
arcKataphron mc = basicEquippedModel kataphronBreacherModel
  & em_rw    .~ [phosphorBlaster, heavyArcRifle mc]
  & em_ccw   .~ arcClaw mc
  & em_name  .~ "arc kataphron breacher"

torsionKataphron :: EquippedModel
torsionKataphron = basicEquippedModel kataphronBreacherModel
  & em_rw    .~ [phosphorBlaster, torsionCannon]
  & em_ccw   .~ hydraulicClaw
  & em_name  .~ "hydraulic/torsion kataphron breacher"

bladesRuststalker :: EquippedModel
bladesRuststalker = basicEquippedModel ruststalkerModel
  & em_ccw    .~ transonicBlades
  & em_name   .~ "ruststalker w/transonic blades"

razorClawRuststalker :: [EquippedModel]
razorClawRuststalker = basicEquippedModel ruststalkerModel
  & em_ccw    .~ transonicRazor
  & em_name   .~ "ruststalker w/transonic razor+claw"
  & splitAttacks 1 chordClaw

razorClawRuststalkerAlpha :: [EquippedModel]
razorClawRuststalkerAlpha = basicEquippedModel (alpha ruststalkerModel)
  & em_ccw    .~ transonicRazor
  & em_name   .~ "ruststalker alpha w/transonic razor+claw"
  & splitAttacks 1 chordClaw

taserGoadInfiltrator :: EquippedModel
taserGoadInfiltrator = basicEquippedModel infiltratorModel
  & em_rw     .~ [flechetteBlaster]
  & em_ccw    .~ taserGoad
  & em_name   .~ "taser goad infiltrator"

powerSwordInfiltrator :: EquippedModel
powerSwordInfiltrator = basicEquippedModel infiltratorModel
  & em_rw     .~ [stubcarbine]
  & em_ccw    .~ powerSword
  & em_name   .~ "power sword infiltrator"

autocannonIronstrider :: EquippedModel
autocannonIronstrider = basicEquippedModel ironstriderModel
  & em_rw    .~ [twin autocannon]
  & em_name  .~ "autocannon ironstrider ballistarius (d-t)"

lascannonIronstrider :: EquippedModel
lascannonIronstrider = basicEquippedModel ironstriderModel
  & em_rw    .~ [twin lascannon]
  & em_name  .~ "lascannon ironstrider ballistarius (d-t)"

taserLanceDragoon :: EquippedModel
taserLanceDragoon = basicEquippedModel dragoonModel
  & em_ccw   .~ taserLance
  & em_name  .~ "sydonian dragoon (taser) (d-t)"

dakkabot :: Protocol -> EquippedModel
dakkabot proto = withProtocol proto $
    basicEquippedModel kastelanModel
      & em_rw   .~ replicate 3 heavyPhosphorBlaster
      & em_name .~ "triple phosphor kastelan"

dakkabots :: Int -> Protocol -> [EquippedModel]
dakkabots n = replicate n . dakkabot


eradicationOnager :: Bool -> EquippedModel
eradicationOnager short = basicEquippedModel onagerModel
  & em_rw   .~ [if short then eradicationBeamerShort else eradicationBeamerLong]
  & em_name .~ "onager dunecrawler (eradication, d-t)"

icarusOnager :: Bool -> EquippedModel
icarusOnager fly = basicEquippedModel onagerModel
  & em_rw   .~ icarusArray fly
  & em_name .~ "onager dunecrawler (icarus array, d-t)"

neutronOnager :: EquippedModel
neutronOnager = basicEquippedModel onagerModel
  & em_rw   .~ [neutronLaser]
  & em_name .~ "onager dunecrawler (neutron laser, d-t)"


-- SQUADS

rangerSquad :: Int -> RngWeapon -> [EquippedModel]
rangerSquad n specialRw =
    rangerWith specialRw
      : rangerWith specialRw
      : (rangerWith galvanicRifle & em_model %~ alpha)
      : replicate (n-3) (rangerWith galvanicRifle)

vanguardSquad :: Int -> RngWeapon -> [EquippedModel]
vanguardSquad n specialRw =
    vanguardWith specialRw
      : vanguardWith specialRw
      : (vanguardWith radiumCarbine & em_model %~ alpha)
      : replicate (n-3) (vanguardWith radiumCarbine)

bladesRuststalkers :: Int -> [EquippedModel]
bladesRuststalkers n =
    (bladesRuststalker & em_model .~ ruststalkerPrincepsModel
                       & em_name  .~ "ruststalker princeps w/transonic blades")
      : replicate (n-1) bladesRuststalker

razorClawRuststalkers :: Int -> [EquippedModel]
razorClawRuststalkers n =
    concat $ razorClawRuststalkerAlpha : replicate (n-1) razorClawRuststalker

taserGoadInfiltrators :: Int -> [EquippedModel]
taserGoadInfiltrators n =
    (taserGoadInfiltrator & em_model .~ infiltratorPrincepsModel
                          & em_name  .~ "taser goad infiltrator princeps")
      : replicate (n-1) taserGoadInfiltrator

powerSwordInfiltrators :: Int -> [EquippedModel]
powerSwordInfiltrators n =
    (powerSwordInfiltrator & em_model .~ infiltratorPrincepsModel
                           & em_name  .~ "power sword infiltrator princeps")
      : replicate (n-1) powerSwordInfiltrator
