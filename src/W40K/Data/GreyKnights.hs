{-# language RankNTypes #-}
{-# language RebindableSyntax #-}
module W40K.Data.GreyKnights where

import Prelude hiding (Functor(..), Monad(..))
import Data.Function (on)
import Data.List (maximumBy)
import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Core.Psychic

import W40K.Data.Common
import qualified W40K.Data.Marines as Marines


-- AURAS

grandMasterMods :: RollMods
grandMasterMods = noMods & mod_rrtohit .~ RerollOnes

draigoMods :: RollMods
draigoMods = noMods & mod_rrtohit .~ RerollFailed

grandMasterAura :: Aura
grandMasterAura = noAura & aura_any <>~ grandMasterMods

draigoAura :: Aura
draigoAura = noAura & aura_any <>~ draigoMods

chaplainAura :: Aura
chaplainAura = noAura & aura_cc.mod_rrtohit .~ RerollFailed


-- MODIFIERS

gkWithSpecialWeapon :: RngWeapon -> Modifier
gkWithSpecialWeapon rw em = em
  & em_rw  .~ [rw]
  & em_ccw .~ basic_ccw

brotherhoodBanner :: Modifier
brotherhoodBanner m = m & em_model.model_att +~ 1 & em_model.model_ld +~ 1

hammerhand :: Modifier
hammerhand = em_model.model_cc_mods.mod_towound +~ 1

psyboltAmmo :: Modifier
psyboltAmmo = em_rw.mapped %~ applyPsybolt
  where
    applyPsybolt rw
      | rw^.rw_name `elem` psyboltWeapons = rw & rw_ap -~ 1 & rw_str +~ 1
      | otherwise                         = rw

    psyboltWeapons = ["boltgun", "storm bolter", "hurricane bolter", "heavy bolter"]

psyOnslaughtAmmo :: Modifier
psyOnslaughtAmmo = em_rw.mapped %~ applyOnslaught
  where
    applyOnslaught rw
      | rw^.rw_name `elem` onslaughtWeapons = rw & rw_ap -~ 1 & rw_str +~ 1
      | otherwise                           = rw

    onslaughtWeapons = ["psilencer", "psycannon", "heavy psycannon", "gatling psilencer"]


heedThePrognosticars :: Model -> Model
heedThePrognosticars = (model_name <>~ " (HtP)") . (model_mods.mod_toinv +~ 1)

sanctuary :: Model -> Model
sanctuary = (model_name <>~ " (sanctuary)") . improveInv model_cc_inv . improveInv model_rng_inv
  where
    improveInv :: Lens' Model Int -> Model -> Model
    improveInv inv_lens m
      | m^.inv_lens <= 3 = m                    -- see FAQ
      | m^.inv_lens >= 7 = m & inv_lens .~ 5
      | otherwise        = m & inv_lens -~ 1

bladeShieldStance :: RollMods -> RollMods
bladeShieldStance = mod_tosave +~ 1

swordStrikeStance :: RollMods -> RollMods
swordStrikeStance = mod_towound +~ 1


-- MODELS

asJusticar :: Model -> Model
asJusticar = (model_ld +~ 1) . (model_att +~ 1) . (model_name <>~ " justicar")

greyKnightModel :: Model
greyKnightModel = meq
  & model_name .~ "grey knight"

purgatorModel :: Model
purgatorModel = greyKnightModel
  & model_name .~ "purgator"

purifierModel :: Model
purifierModel = greyKnightModel
  & model_ld   +~ 1
  & model_name .~ "purifier"

terminatorModel :: Model
terminatorModel = teq
  & model_ld   -~ 1
  & model_name .~ "grey knight terminator"

paladinModel :: Model
paladinModel = terminatorModel
  & model_att  .~ 3
  & model_ld   +~ 1
  & model_wnd  .~ 3
  & model_name .~ "grey knight paladin"

paragonModel :: Model
paragonModel = paladinModel
  & model_ws   .~ 2
  & model_ld   +~ 1
  & model_name .~ "grey knight paragon"

apothecaryModel :: Model
apothecaryModel = terminatorModel
  & model_wnd  .~ 5
  & model_att  .~ 4
  & model_ld   .~ 8
  & model_name .~ "grey knight apothecary"

brotherhoodChampionModel :: (RollMods -> RollMods) -> Model
brotherhoodChampionModel stance = greyKnightModel
  & model_ws      .~ 2
  & model_bs      .~ 2
  & model_wnd     .~ 4
  & model_att     .~ 4
  & model_ld      .~ 8
  & model_save    .~ 2
  & model_inv     .~ 4
  & model_cc_mods %~ stance
  & model_name    .~ "brotherhood champion"

castellanCroweModel :: Model
castellanCroweModel = greyKnightModel
  & model_ws   .~ 2
  & model_bs   .~ 2
  & model_wnd  .~ 5
  & model_att  .~ 5
  & model_ld   .~ 8
  & model_save .~ 2
  & model_inv  .~ 4
  & model_cc_mods.mod_rrtohit   .~ RerollFailed
  & model_cc_mods.mod_rrtowound .~ RerollFailed

grandMasterModel :: Model
grandMasterModel = terminatorModel
  & model_ws   .~ 2
  & model_bs   .~ 2
  & model_wnd  .~ 6
  & model_att  .~ 5
  & model_ld   .~ 9
  & model_inv  .~ 4
  & model_mods <>~ grandMasterMods
  & model_name .~ "grand master"

gmndkModel :: Model
gmndkModel = grandMasterModel
  & model_str  .~ 6
  & model_tgh  .~ 6
  & model_wnd  .~ 12
  & model_name .~ "GMNDK"

draigoModel :: Model
draigoModel = grandMasterModel
  & model_wnd  .~ 7
  & model_inv  .~ 3
  & model_mods <>~ draigoMods
  & model_name .~ "lord kaldor draigo"

-- PSYCHIC

gkCasting :: Bool -> [EquippedModel] -> PsykerCasting
gkCasting channeling models = PsykerCasting
    { _cast_bonus                  = Add 1
    , _cast_usingPsychicChanneling = channeling
    , _cast_psyker                 = maximumBy (compare `on` (^.em_model.model_ld)) models
                                       ^. em_model
    }

ritesOfBanishment :: PsychicPower
ritesOfBanishment = PsychicPower
    { _power_castingValue = 5
    , _power_inflictMortalWounds = \_ _ _ -> return 1
    }

cleansingFlame :: PsychicPower
cleansingFlame = PsychicPower
    { _power_castingValue = 5
    , _power_inflictMortalWounds = \_ _ _ -> d6
    }

vortexOfDoom :: PsychicPower
vortexOfDoom = PsychicPower
    { _power_castingValue = 8
    , _power_inflictMortalWounds = \_ _ cv -> if cv >= 12 then d6 else d3
    }

purgeSoul :: PsychicPower
purgeSoul = PsychicPower
    { _power_castingValue = 5
    , _power_inflictMortalWounds = \src tgt _ -> do
        diff <- liftA2 (-) d6 d6
        return $ max 0 (src^.model_ld - tgt^.model_ld + diff)
    }


-- RANGED WEAPONS

psilencer :: RngWeapon
psilencer = bolter
  & rw_shots .~ return 6
  & rw_class .~ Heavy
  & rw_dmg   .~ d3
  & rw_name  .~ "psilencer"

psycannon :: RngWeapon
psycannon = bolter
  & rw_shots .~ return 4
  & rw_str   .~ 7
  & rw_class .~ Heavy
  & rw_ap    .~ -1
  & rw_name  .~ "psycannon"

incinerator :: RngWeapon
incinerator = bolter
  & rw_shots   .~ d6
  & rw_str     .~ 6
  & rw_class   .~ Assault
  & rw_autohit .~ True
  & rw_ap      .~ -1
  & rw_name    .~ "incinerator"

heavyPsycannon :: RngWeapon
heavyPsycannon = psycannon
  & rw_dmg  .~ return 2
  & rw_name .~ "heavy psycannon"


-- CC WEAPONS

nemesis_ccw :: CCWeapon
nemesis_ccw = basic_ccw
  & ccw_dmg  .~ d3
  & ccw_name .~ "(nemesis weapon)"

nemesisSword :: CCWeapon
nemesisSword = forceSword
  & ccw_name .~ "nemesis sword"

falchion :: CCWeapon
falchion = nemesis_ccw
  & ccw_attBonus .~ NoMod
  & ccw_ap       .~ -2
  & ccw_name     .~ "nemesis falchion"

twoFalchions :: CCWeapon
twoFalchions = falchion
  & ccw_attBonus .~ Add 1
  & ccw_name     .~ "two nemesis falchions"

halberd :: CCWeapon
halberd = nemesis_ccw
  & ccw_strMod .~ Add 1
  & ccw_ap     .~ -2
  & ccw_name   .~ "nemesis halberd"

wardingStave :: CCWeapon
wardingStave = nemesis_ccw
  & ccw_strMod .~ Add 2
  & ccw_ap     .~ -1
  & ccw_name   .~ "nemesis warding stave"

hammer :: CCWeapon
hammer = basic_ccw
  & ccw_strMod    .~ Times 2
  & ccw_ap        .~ -3
  & ccw_dmg       .~ return 3
  & ccw_name      .~ "nemesis daemon hammer"
  & makeUnwieldly

blackBladeAntwyr :: CCWeapon
blackBladeAntwyr = basic_ccw
  & ccw_weapon.w_hooks.hook_wound %~ addRollHook 0 (WoundHookExtraAttacks 1)
  & ccw_name                      .~ "black blade of antwyr"

soulGlaive :: CCWeapon
soulGlaive = halberd
  & ccw_ap   .~ -3
  & ccw_mods .~ (noMods & mod_rrtowound .~ RerollFailed)
  & ccw_name .~ "soul glaive"

crys'yllixDestroyer :: CCWeapon
crys'yllixDestroyer = hammer
  & ccw_dmg  .~ return 4
  & ccw_name .~ "destroyer of crys'yllix"

argyrum :: CCWeapon
argyrum = basic_ccw
  & ccw_strMod    .~ Times 2
  & ccw_ap        .~ -3
  & ccw_dmg       .~ return 3
  & ccw_name      .~ "malleus argyrum"

titansword :: CCWeapon
titansword = nemesis_ccw
  & ccw_strMod .~ Add 4
  & ccw_ap     .~ -4
  & ccw_dmg    .~ return 3
  & ccw_name   .~ "the titansword"

greatsword :: CCWeapon
greatsword = argyrum
  & ccw_strMod .~ Add 4
  & ccw_dmg    .~ d6
  & ccw_name   .~ "nemesis greatsword"

greathammer :: CCWeapon
greathammer = greatsword
  & ccw_strMod    .~ Times 2
  & ccw_dmg       .~ fmap (max 3) d6
  & ccw_name      .~ "nemesis greathammer"
  & makeUnwieldly

nemesisDoomglaive :: CCWeapon
nemesisDoomglaive = nemesis_ccw
  & ccw_strMod .~ Add 3
  & ccw_ap     .~ -3
  & ccw_dmg    .~ d6
  & ccw_name   .~ "nemesis doomglaive"


-- EQUIPPED MODELS

gkEquippedModel :: Model -> CCWeapon -> EquippedModel
gkEquippedModel m ccw = basicEquippedModel m
  & em_ccw .~ ccw
  & em_rw  .~ [stormBolter]


greyKnight :: CCWeapon -> EquippedModel
greyKnight = gkEquippedModel greyKnightModel

greyKnightJusticar :: CCWeapon -> EquippedModel
greyKnightJusticar = gkEquippedModel (asJusticar greyKnightModel)

purgator :: RngWeapon -> EquippedModel
purgator rw = basicEquippedModel purgatorModel
  & em_rw .~ [rw]

purgatorJusticar :: CCWeapon -> EquippedModel
purgatorJusticar = gkEquippedModel (asJusticar purgatorModel)

purifier :: CCWeapon -> EquippedModel
purifier ccw = gkEquippedModel purifierModel ccw

purifierJusticar :: CCWeapon -> EquippedModel
purifierJusticar = gkEquippedModel (asJusticar purifierModel & model_name .~ "knight of the flame")

terminator :: CCWeapon -> EquippedModel
terminator = gkEquippedModel terminatorModel

terminatorJusticar :: CCWeapon -> EquippedModel
terminatorJusticar = gkEquippedModel (asJusticar terminatorModel)

paladin :: CCWeapon -> EquippedModel
paladin = gkEquippedModel paladinModel

paragon :: CCWeapon -> EquippedModel
paragon = gkEquippedModel paragonModel

apothecary :: CCWeapon -> EquippedModel
apothecary ccw = basicEquippedModel apothecaryModel
  & em_ccw .~ ccw

brotherhoodChampion :: (RollMods -> RollMods) -> EquippedModel
brotherhoodChampion stance = gkEquippedModel (brotherhoodChampionModel stance) nemesisSword

castellanCrowe :: EquippedModel
castellanCrowe = gkEquippedModel castellanCroweModel blackBladeAntwyr

brotherhoodAncient :: EquippedModel
brotherhoodAncient = paladin falchion
  & em_model.model_wnd .~ 5
  & em_name            .~ "brotherhood ancient"

grandMaster :: CCWeapon -> EquippedModel
grandMaster = gkEquippedModel grandMasterModel

gmndkWith :: [RngWeapon] -> CCWeapon -> EquippedModel
gmndkWith rw ccw = basicEquippedModel gmndkModel
  & em_rw  .~ rw
  & em_ccw .~ ccw

voldus :: EquippedModel
voldus = grandMaster argyrum
  & em_name .~ "grand master voldus"

draigo :: EquippedModel
draigo = gkEquippedModel draigoModel titansword

doomglaiveDread :: EquippedModel
doomglaiveDread = basicEquippedModel Marines.venDreadnought
  & em_rw    .~ [stormBolter, heavyPsycannon]
  & em_ccw   .~ nemesisDoomglaive
  & em_name  .~ "doomglaive dreadnought"


-- SQUADS

strikeSquad :: Int -> CCWeapon -> [EquippedModel]
strikeSquad n ccw = greyKnightJusticar ccw : replicate (n-1) (greyKnight ccw)

specialWeaponPurifierSquad5 :: RngWeapon -> CCWeapon -> [EquippedModel]
specialWeaponPurifierSquad5 rw ccw =
    [ purifier ccw & gkWithSpecialWeapon rw
    , purifier ccw & gkWithSpecialWeapon rw
    , purifier ccw
    , purifier ccw
    , purifierJusticar ccw
    ]

specialWeaponStrikeSquad5 :: RngWeapon -> CCWeapon -> [EquippedModel]
specialWeaponStrikeSquad5 rng ccw =
    (greyKnight ccw & em_rw  .~ [rng]
                    & em_ccw .~ basic_ccw)
      : replicate 3 (greyKnight ccw)
      ++ [greyKnightJusticar ccw]

specialWeaponStrikeSquad10 :: RngWeapon -> CCWeapon -> [EquippedModel]
specialWeaponStrikeSquad10 rng ccw =
    replicate 2 (greyKnight ccw & em_rw  .~ [rng]
                                & em_ccw .~ basic_ccw)
      ++ replicate 7 (greyKnight ccw)
      ++ [greyKnightJusticar ccw]

purgatorSquad :: RngWeapon -> [EquippedModel]
purgatorSquad rw = purgatorJusticar twoFalchions : replicate 4 (purgator rw)

terminatorSquad :: Int -> CCWeapon -> [EquippedModel]
terminatorSquad n ccw = terminatorJusticar ccw : replicate (n-1) (terminator ccw)

paladinSquad :: Int -> CCWeapon -> [EquippedModel]
paladinSquad n ccw = paragon ccw : replicate (n-1) (paladin ccw)
