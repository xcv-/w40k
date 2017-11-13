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
import W40K.Data.Marines


-- AURAS

grandMasterMods :: RollMods
grandMasterMods = noMods & mod_rrtohit .~ RerollOnes

draigoMods :: RollMods
draigoMods = noMods & mod_rrtohit .~ RerollFailed

grandMasterAura :: Aura
grandMasterAura = noAura & aura_any <>~ grandMasterMods

draigoAura :: Aura
draigoAura = noAura & aura_any <>~ draigoMods


-- MODIFIERS

gkWithSpecialWeapon :: RngWeapon -> EquippedModel -> EquippedModel
gkWithSpecialWeapon rw em = em
  & em_rw  .~ rw
  & em_ccw .~ basic_ccw

brotherhoodBanner :: EquippedModel -> EquippedModel
brotherhoodBanner m = m & em_model.model_att +~ 1 & em_model.model_ld +~ 1

hammerhanded :: [EquippedModel] -> [EquippedModel]
hammerhanded = map $ em_model.model_cc_mods.mod_towound +~ 1

psyboltAmmo :: EquippedModel -> EquippedModel
psyboltAmmo = em_rw %~ applyPsybolt
  where
    applyPsybolt rw
      | rw^.rw_name `elem` psyboltWeapons = rw & rw_ap -~ 1 & rw_str +~ 1
      | otherwise                         = rw

    psyboltWeapons = ["boltgun", "storm bolter", "hurricane bolter", "heavy bolter"]

psyOnslaughtAmmo :: EquippedModel -> EquippedModel
psyOnslaughtAmmo = em_rw %~ applyOnslaught
  where
    applyOnslaught rw
      | rw^.rw_name `elem` onslaughtWeapons = rw & rw_ap -~ 1 & rw_str +~ 1
      | otherwise                           = rw

    onslaughtWeapons = ["psilencer", "psycannon", "heavy psycannon", "gatling psilencer"]


bladeShieldStance :: RollMods -> RollMods
bladeShieldStance = mod_tosave +~ 1

swordStrikeStance :: RollMods -> RollMods
swordStrikeStance = mod_towound +~ 1

-- PSYCHIC

gkCasting :: Bool -> [EquippedModel] -> PsychicPower -> PsykerCasting
gkCasting channeling models power = PsykerCasting
    { _cast_bonus                  = Add 1
    , _cast_usingPsychicChanneling = channeling
    , _cast_psyker                 = maximumBy (compare `on` (^.em_model.model_ld)) models
                                       ^. em_model
    , _cast_power                  = power
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
  & ccw_unwieldly .~ True
  & ccw_name      .~ "nemesis daemon hammer"

blackBladeAntwyr :: CCWeapon
blackBladeAntwyr = basic_ccw
  & ccw_weapon.w_hooks.hook_wound .~ Just (RollHook 0 (WoundHookExtraAttacks 1))
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
argyrum = hammer
  & ccw_unwieldly .~ False
  & ccw_name      .~ "malleus argyrum"

titansword :: CCWeapon
titansword = nemesis_ccw
  & ccw_strMod .~ Add 4
  & ccw_ap     .~ -4
  & ccw_dmg    .~ return 3
  & ccw_name   .~ "the titansword"

nemesisDoomglaive :: CCWeapon
nemesisDoomglaive = nemesis_ccw
  & ccw_strMod .~ Add 3
  & ccw_ap     .~ -3
  & ccw_dmg    .~ d6
  & ccw_name   .~ "nemesis doomglaive"

-- EQUIPPED MODELS

greyKnight :: CCWeapon -> EquippedModel
greyKnight ccw = basicEquippedModel meq
  & em_ccw   .~ ccw
  & em_rw    .~ stormBolter
  & em_name  .~ "grey knight"

purgator :: RngWeapon -> EquippedModel
purgator rw = basicEquippedModel meq
  & em_rw    .~ rw
  & em_name  .~ "purgator"

justicar :: CCWeapon -> EquippedModel
justicar ccw = greyKnight ccw
  & em_model.model_att +~ 1
  & em_model.model_ld  +~ 1
  & em_name            .~ "grey knight justicar"

purifier :: CCWeapon -> EquippedModel
purifier ccw = greyKnight ccw
  & em_model .~ (meq & model_ld +~ 1)
  & em_name  .~ "purifier"

purifierJusticar :: CCWeapon -> EquippedModel
purifierJusticar ccw = purifier ccw
  & em_model.model_att +~ 1
  & em_model.model_ld  +~ 1
  & em_name            .~ "purifier justicar"

gkTerminator :: CCWeapon -> EquippedModel
gkTerminator ccw = basicEquippedModel teq
  & em_model.model_ld -~ 1
  & em_ccw            .~ ccw
  & em_rw             .~ stormBolter
  & em_name           .~ "grey knight terminator"

gkTerminatorJusticar :: CCWeapon -> EquippedModel
gkTerminatorJusticar ccw = gkTerminator ccw
  & em_model.model_att +~ 1
  & em_model.model_ld  +~ 1
  & em_name            .~ "grey knight terminator justicar"

paladin :: CCWeapon -> EquippedModel
paladin ccw = gkTerminator ccw
  & em_model .~ (teq & model_att .~ 3
                     & model_wnd .~ 3)
  & em_name  .~ "grey knight paladin"

paragon :: CCWeapon -> EquippedModel
paragon ccw = paladin ccw
  & em_model.model_ws .~ 2
  & em_model.model_ld +~ 1
  & em_name           .~ "grey knight paragon"

gkApothecary :: CCWeapon -> EquippedModel
gkApothecary ccw = paragon ccw
  & em_model.model_wnd .~ 5
  & em_model.model_att .~ 4
  & em_model.model_ld  .~ 8
  & em_rw              .~ null_rw
  & em_name            .~ "grey knight apothecary"

brotherhoodChampion :: (RollMods -> RollMods) -> EquippedModel
brotherhoodChampion stance = greyKnight nemesisSword
  & em_model.model_ws      .~ 2
  & em_model.model_bs      .~ 2
  & em_model.model_wnd     .~ 4
  & em_model.model_att     .~ 4
  & em_model.model_ld      .~ 8
  & em_model.model_save    .~ 2
  & em_model.model_inv     .~ 4
  & em_model.model_cc_mods %~ stance
  & em_model.model_name    .~ "brotherhood champion"

castellanCrowe :: EquippedModel
castellanCrowe = greyKnight blackBladeAntwyr
  & em_model.model_ws   .~ 2
  & em_model.model_bs   .~ 2
  & em_model.model_wnd  .~ 5
  & em_model.model_att  .~ 5
  & em_model.model_ld   .~ 8
  & em_model.model_save .~ 2
  & em_model.model_inv  .~ 4
  & em_model.model_cc_mods.mod_rrtohit   .~ RerollFailed
  & em_model.model_cc_mods.mod_rrtowound .~ RerollFailed

brotherhoodAncient :: EquippedModel
brotherhoodAncient = paladin falchion
  & em_model.model_wnd .~ 5
  & em_name            .~ "brotherhood ancient"

grandMaster :: CCWeapon -> EquippedModel
grandMaster ccw = gkTerminator ccw
  & em_model.model_ws   .~ 2
  & em_model.model_bs   .~ 2
  & em_model.model_wnd  .~ 6
  & em_model.model_att  .~ 5
  & em_model.model_ld   .~ 9
  & em_model.model_inv  .~ 4
  & em_model.model_mods <>~ grandMasterMods
  & em_ccw              .~ ccw
  & em_name             .~ "grand master"

voldus :: EquippedModel
voldus = grandMaster argyrum
  & em_name .~ "grand master voldus"

draigo :: EquippedModel
draigo = grandMaster titansword
  & em_model.model_wnd  .~ 7
  & em_model.model_inv  .~ 3
  & em_model.model_mods <>~ draigoMods
  & em_name             .~ "lord kaldor draigo"

doomglaiveDread :: EquippedModel
doomglaiveDread = basicEquippedModel venDreadnought
  & em_rw    .~ heavyPsycannon
  & em_ccw   .~ nemesisDoomglaive
  & em_name  .~ "doomglaive dreadnought"

-- SQUADS

strikeSquad :: Int -> CCWeapon -> [EquippedModel]
strikeSquad n ccw = justicar ccw : replicate (n-1) (greyKnight ccw)

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
    (greyKnight ccw & em_rw .~ rng & em_ccw .~ basic_ccw)
      : replicate 3 (greyKnight ccw)
      ++ [justicar ccw]

specialWeaponStrikeSquad10 :: RngWeapon -> CCWeapon -> [EquippedModel]
specialWeaponStrikeSquad10 rng ccw =
    replicate 2 (greyKnight ccw & em_rw .~ rng & em_ccw .~ basic_ccw)
      ++ replicate 7 (greyKnight ccw)
      ++ [justicar ccw]

purgatorSquad :: RngWeapon -> [EquippedModel]
purgatorSquad rw = justicar twoFalchions : replicate 4 (purgator rw)

gkTerminatorSquad :: Int -> CCWeapon -> [EquippedModel]
gkTerminatorSquad n ccw = gkTerminatorJusticar ccw : replicate (n-1) (gkTerminator ccw)

paladinSquad :: Int -> CCWeapon -> [EquippedModel]
paladinSquad n ccw = paragon ccw : replicate (n-1) (paladin ccw)
