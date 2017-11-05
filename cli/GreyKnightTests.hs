module GreyKnightTests where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import W40K.Core.Prob
import W40K.Core.Mechanics
import W40K.Core.Chart
import W40K.Core.Psychic

import W40K.Data.Common
import W40K.Data.GreyKnights
import W40K.Data.Marines


andAuras :: [NamedEqUnit] -> [NamedEqUnit]
andAuras squads =
    squads ++ concatMap (\(name, aura) -> modNamedEqUnit name aura squads)
                        [("GM", within grandMasterAura), ("draigo", within draigoAura)]

andBanner :: [NamedEqUnit] -> [NamedEqUnit]
andBanner squads =
    squads ++ modNamedEqUnit "banner" (with brotherhoodBanner) squads

andPsybolt :: [NamedEqUnit] -> [NamedEqUnit]
andPsybolt squads =
    squads ++ modNamedEqUnit "psybolt" (with psyboltAmmo) squads

andPsyOnslaught :: [NamedEqUnit] -> [NamedEqUnit]
andPsyOnslaught squads =
    squads ++ modNamedEqUnit "onslaught" (with psyOnslaughtAmmo) squads


godhammerAvTest :: [NamedEqUnit]
godhammerAvTest = andAuras $
    setCombatType Ranged
      [ ("LR lascannons",        godhammerLandraider)
      , ("LR lascannons, melta", fullAvGodhammerLandraider)
      ]

stormravenAvTest :: [NamedEqUnit]
stormravenAvTest = andAuras $
    setCombatType Ranged
      [
        ("SR lascannons, melta",                closeEnough $ [stormraven's (twin multimelta), stormraven's (twin lascannon)])
      , ("SR lascannons, melta, stormstrikes",  closeEnough fullAvStormraven)
      , ("SR plasma, melta, stormstrikes",      closeEnough $ two (stormraven's stormstrike) ++ [stormraven's (twin heavyPlasmaCannon), stormraven's (twin multimelta)])
      , ("SR melta, stormstrikes",              closeEnough meltaStormraven)
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
terminatorAvTest = andBanner $ hqCcAvTest ("5 hammer termies", gkTerminatorSquad 5 hammer)

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
      [ ("5 termies " ++ ccw^.ccw_name, gkTerminatorSquad 5 ccw) | ccw <- [nemesisSword, twoFalchions, halberd, wardingStave]]

smallPaladinCcInfTest :: [NamedEqUnit]
smallPaladinCcInfTest = andAuras $ andBanner $
    setCombatType Melee
      [ ("3 paladins " ++ ccw^.ccw_name, paladinSquad 3 ccw) | ccw <- [nemesisSword, twoFalchions, halberd, wardingStave]]

smallStrikeRngInfTest :: [NamedEqUnit]
smallStrikeRngInfTest = andAuras $
    (andPsybolt . setCombatType Ranged)
      [ ("5 strikes",                               moving $ rapidFiring $ strikeSquad 5                         nemesisSword)
      , ("5 strikes (1 psilencer)",                 moving $ rapidFiring $ specialWeaponStrikeSquad5   psilencer nemesisSword)
      , ("5 strikes (1 psycannon)",                 moving $ rapidFiring $ specialWeaponStrikeSquad5   psycannon nemesisSword)
      ]

purifierRngInfTest :: [NamedEqUnit]
purifierRngInfTest =
    setCombatType Ranged
      [ ("5 purifiers (2 psilencers)",              moving $ rapidFiring $ specialWeaponPurifierSquad5 psilencer nemesisSword)
      , ("5 purifiers (2 psycannons)",              moving $ rapidFiring $ specialWeaponPurifierSquad5 psycannon nemesisSword)
      ]

purgatorRngInfTest :: [NamedEqUnit]
purgatorRngInfTest = andAuras $
    (andPsyOnslaught . setCombatType Ranged)
      [ ("5 purgators (4 psilencers)",              moving $ rapidFiring $ purgatorSquad              psilencer)
      , ("5 purgators (4 psycannons)",              moving $ rapidFiring $ purgatorSquad              psycannon)
      --, ("5 purgators (stationary) (4 psilencers)",          rapidFiring $ purgatorSquad              psilencer)
      --, ("5 purgators (stationary) (4 psycannons)",          rapidFiring $ purgatorSquad              psycannon)
      ]

largePagkRngInfTest :: [NamedEqUnit]
largePagkRngInfTest = andAuras $
    (andPsybolt . setCombatType Ranged)
      [ ("10 strikes",                 moving $ rapidFiring $ strikeSquad 10 nemesisSword)
      -- , ("10 strikes (2 psilencers)",  moving $ rapidFiring $ specialWeaponStrikeSquad10 psilencer nemesisSword)
      , ("10 strikes (2 psycannons)",  moving $ rapidFiring $ specialWeaponStrikeSquad10 psycannon nemesisSword)
      ]

crusaderInfTest :: [NamedEqUnit]
crusaderInfTest = andAuras $
    (andPsybolt . setCombatType Ranged)
      [ ("LR crusader (no melta)", rapidFiring $ landraider's (twin assaultCannon) : two (landraider's hurricaneBolter))
      , ("LR crusader (HB only)",  rapidFiring $ two (landraider's hurricaneBolter))
      , ("LR crusader (AC only)",  rapidFiring $ [landraider's (twin assaultCannon)])
      ]

-- fullSmallGkInfTest :: [NamedEqUnit]
-- fullSmallGkInfTest = smallStrikeRngInfTest ++ purifierRngInfTest ++ purgatorRngInfTest ++ smallGkCcInfTest
