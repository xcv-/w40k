module GreyKnightTests where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Core.Chart
import W40K.Core.Psychic

import W40K.Data.Common
import W40K.Data.GreyKnights
import qualified W40K.Data.Marines as Marines


andAuras :: [NamedEqUnit] -> [NamedEqUnit]
andAuras squads =
    squads ++ concatMap (\(name, aura) -> mapNamedEqUnit name aura squads)
                        [("GM", within grandMasterAura), ("draigo", within draigoAura)]

andBanner :: [NamedEqUnit] -> [NamedEqUnit]
andBanner squads =
    squads ++ mapNamedEqUnit "banner" (with brotherhoodBanner) squads

andPsybolt :: [NamedEqUnit] -> [NamedEqUnit]
andPsybolt squads =
    squads ++ mapNamedEqUnit "psybolt" (with psyboltAmmo) squads

andPsyOnslaught :: [NamedEqUnit] -> [NamedEqUnit]
andPsyOnslaught squads =
    squads ++ mapNamedEqUnit "onslaught" (with psyOnslaughtAmmo) squads


godhammerAvTest :: [NamedEqUnit]
godhammerAvTest = andAuras $
    setCombatType Ranged
      [ ("LR lascannons",        Marines.godhammerLandraider)
      , ("LR lascannons, melta", Marines.fullAvGodhammerLandraider)
      ]

stormravenAvTest :: [NamedEqUnit]
stormravenAvTest = andAuras $
    setCombatType Ranged
      [
        ("SR lascannons, melta",                with closeEnoughRange $ [Marines.stormraven's (twin multimelta), Marines.stormraven's (twin lascannon)])
      , ("SR lascannons, melta, stormstrikes",  with closeEnoughRange Marines.fullAvStormraven)
      , ("SR plasma, melta, stormstrikes",      with closeEnoughRange $ two [Marines.stormraven's Marines.stormstrike] ++ [Marines.stormraven's (twin heavyPlasmaCannon), Marines.stormraven's (twin multimelta)])
      , ("SR melta, stormstrikes",              with closeEnoughRange Marines.meltaStormraven)
      ]

hqCcAvTest :: (String, [EquippedModel]) -> [NamedEqUnit]
hqCcAvTest (name, squad) =
    setCombatType Melee
      [ (name,                     squad)
      , (name ++ ", grand master", within grandMasterAura $ grandMaster hammer : squad)
      , (name ++ ", voldus",       within grandMasterAura $ voldus : squad)
      , (name ++ ", draigo",       within draigoAura      $ draigo : squad)
      ]

terminatorAvTest :: [NamedEqUnit]
terminatorAvTest = andBanner $ hqCcAvTest ("5 hammer termies", terminatorSquad 5 hammer)

paladinAvTest :: [NamedEqUnit]
paladinAvTest = andBanner $ hqCcAvTest ("3 hammer paladins", paladinSquad 3 hammer)

fullGkAvTest :: [NamedEqUnit]
fullGkAvTest = godhammerAvTest ++ stormravenAvTest ++ paladinAvTest ++ terminatorAvTest


smallPagkCcInfTest :: [NamedEqUnit]
smallPagkCcInfTest = andAuras $ andBanner $
    setCombatType Melee
      [ ("5 strikes " ++ ccw^.ccw_name, strikeSquad 5 ccw) | ccw <- [nemesisSword, twoFalchions, halberd, wardingStave]]

smallTagkCcInfTest :: [NamedEqUnit]
smallTagkCcInfTest = andAuras $ andBanner $
    setCombatType Melee
      [ ("5 termies " ++ ccw^.ccw_name, terminatorSquad 5 ccw) | ccw <- [nemesisSword, twoFalchions, halberd, wardingStave]]

smallPaladinCcInfTest :: [NamedEqUnit]
smallPaladinCcInfTest = andAuras $ andBanner $
    setCombatType Melee
      [ ("3 paladins " ++ ccw^.ccw_name, paladinSquad 3 ccw) | ccw <- [nemesisSword, twoFalchions, halberd, wardingStave]]

smallStrikeRngInfTest :: [NamedEqUnit]
smallStrikeRngInfTest = andAuras $ andPsybolt $
    setCombatType Ranged
      [ ("5 strikes",                               with (moving.rapidFireRange) $ strikeSquad 5                         nemesisSword)
      , ("5 strikes (1 psilencer)",                 with (moving.rapidFireRange) $ specialWeaponStrikeSquad5   psilencer nemesisSword)
      , ("5 strikes (1 psycannon)",                 with (moving.rapidFireRange) $ specialWeaponStrikeSquad5   psycannon nemesisSword)
      ]

purifierRngInfTest :: [NamedEqUnit]
purifierRngInfTest =
    setCombatType Ranged
      [ ("5 purifiers (2 psilencers)",              with (moving.rapidFireRange) $ specialWeaponPurifierSquad5 psilencer nemesisSword)
      , ("5 purifiers (2 psycannons)",              with (moving.rapidFireRange) $ specialWeaponPurifierSquad5 psycannon nemesisSword)
      ]

purgatorRngInfTest :: [NamedEqUnit]
purgatorRngInfTest = andAuras $ andPsyOnslaught $
    setCombatType Ranged
      [ ("5 purgators (4 psilencers)",              with (moving.rapidFireRange) $ purgatorSquad              psilencer)
      , ("5 purgators (4 psycannons)",              with (moving.rapidFireRange) $ purgatorSquad              psycannon)
      --, ("5 purgators (stationary) (4 psilencers)",          rapidFiring $ purgatorSquad              psilencer)
      --, ("5 purgators (stationary) (4 psycannons)",          rapidFiring $ purgatorSquad              psycannon)
      ]

largePagkRngInfTest :: [NamedEqUnit]
largePagkRngInfTest = andAuras $ andPsybolt $
    setCombatType Ranged
      [ ("10 strikes",                 with (moving.rapidFireRange) $ strikeSquad 10 nemesisSword)
      -- , ("10 strikes (2 psilencers)",  with (moving.rapidFireRange) $ specialWeaponStrikeSquad10 psilencer nemesisSword)
      , ("10 strikes (2 psycannons)",  with (moving.rapidFireRange) $ specialWeaponStrikeSquad10 psycannon nemesisSword)
      ]

crusaderInfTest :: [NamedEqUnit]
crusaderInfTest = andAuras $ andPsybolt $
    setCombatType Ranged
      [ ("LR crusader (no melta)", with rapidFireRange $ Marines.landraider's (twin assaultCannon) : two [Marines.landraider's hurricaneBolter])
      , ("LR crusader (HB only)",  with rapidFireRange $ two [Marines.landraider's hurricaneBolter])
      , ("LR crusader (AC only)",  with rapidFireRange $ [Marines.landraider's (twin assaultCannon)])
      ]

-- fullSmallGkInfTest :: [NamedEqUnit]
-- fullSmallGkInfTest = smallStrikeRngInfTest ++ purifierRngInfTest ++ purgatorRngInfTest ++ smallGkCcInfTest
