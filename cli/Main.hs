{-# language RebindableSyntax #-}
module Main where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import Data.List (isInfixOf)

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Chart (AnalysisConfig(..), AnalysisOrder(..), ProbPlotType(..), AnalysisFn(..))
import qualified W40K.Core.Chart.R as R
import W40K.Core.Mechanics
import W40K.Core.Psychic

import W40K.Data.Common
import qualified W40K.Data.AdMech          as AdMech
import qualified W40K.Data.Assassins       as Assassins
import qualified W40K.Data.Chaos           as Chaos
import qualified W40K.Data.DeathGuard      as DG
import qualified W40K.Data.Eldar           as Eldar
import qualified W40K.Data.GreyKnights     as GK
import qualified W40K.Data.ImperialKnights as IK
import qualified W40K.Data.Inquisition     as Inq
import qualified W40K.Data.Marines         as Marines
import qualified W40K.Data.Necrons         as Necrons
import qualified W40K.Data.Tau             as Tau
import qualified W40K.Data.ThousandSons    as TSons
import qualified W40K.Data.Tyranids        as Tyranids


main :: IO ()
main = R.withEmbeddedR $ do
    chaosPrimarchTests
    plagueMarineTests
    arcWeaponryTests


chaosPrimarchTests :: IO ()
chaosPrimarchTests = do
    R.analysisToFile "/tmp/primarchs-mixed.png"
      [ AnalysisConfig ByTarget ProbKillOne                        mixedAttackers chaosPrimarchs
      , AnalysisConfig ByTarget WoundingSummary                    mixedAttackers chaosPrimarchs
      , AnalysisConfig ByTarget (NumWoundsMax RevDistributionPlot) mixedAttackers chaosPrimarchs
      ]

    R.analysisToFile "/tmp/primarchs-knights.png"
      [ AnalysisConfig ByTarget ProbKillOne                        bigKnights     chaosPrimarchs
      , AnalysisConfig ByTarget WoundingSummary                    bigKnights     chaosPrimarchs
      , AnalysisConfig ByTarget (NumWoundsMax RevDistributionPlot) bigKnights     chaosPrimarchs
      ]

    return ()
  where
    chaosPrimarchs =
        [ DG.mortarion       DG.silenceReaping
        , DG.buffedMortarion DG.silenceReaping
        , TSons.magnus
        , TSons.buffedMagnus
        ]

    denier tgt
      | "magnus" `isInfixOf` (tgt^.model_name) = Just TSons.magnusPsyker
      | otherwise                              = Just defaultPsyker

    mixedAttackers =
      [
        -- GK
        mconcat [
          psychicMod GK.gkPsyker GK.hammerhand denier $
          deepstriking RerollChargeAnyDice $
          eraseTurn emptyTurn {
            turnName = "DS GMNDK (shooting+FttF)",

            turnShooting = \_ -> [GK.gmndk],

            turnMelee = \_ _ ->
               [GK.gmndk & em_model.model_cc_mods.mod_rrtowound <>~ RerollFailed]
          },

          eraseTurn emptyTurn {
              turnName = "draigo flyerwing",
              turnShooting = \_ ->
                with moving $ within GK.draigoAura
                  [ Marines.stormravenWith [lascannon, Marines.stormstrike, Marines.stormstrike]
                  , Marines.stormhawkWith [Marines.lastalon] & em_model %~ Marines.interceptorJet
                  , Marines.stormhawkWith [Marines.lastalon] & em_model %~ Marines.interceptorJet
                  ]
          }
        ],

        -- AdMech/IK
        mconcat [
          eraseTurn emptyTurn {
            turnName = "2 helverin",
            turnShooting = \_ ->
              [ IK.armigerHelverin
              , IK.armigerHelverin
              ]
          },

          eraseTurn emptyTurn {
            turnName = "headman's mark paladin w/stormspear",
            turnShooting = \_ -> [IK.headman'sMark (IK.knightPaladin [IK.stormspearRocketPod] IK.reaperChainsword)],
            turnMelee = \_ _ -> [IK.stomping $ IK.headman'sMark (IK.knightPaladin [IK.stormspearRocketPod] IK.reaperChainsword)]
          },

          eraseTurn emptyTurn {
            turnName = "3 rr1 neutronagers",
            turnShooting = \_ ->
              within AdMech.cawlAura $
                replicate 3 AdMech.neutronOnager
          },

          chargeFilter (chargeRoll RerollChargeOneDie (24 - 9 - 10)) $
          eraseTurn emptyTurn {
            turnName = "3 protector rr1 AC ballistarii",
            turnShooting = \_ ->
              within AdMech.cawlAura $
                replicate 3 AdMech.autocannonIronstrider
          }
        ],

        mconcat [
          eraseTurn emptyTurn {
            turnName = "atropos (shooting+lascutter)",
            turnShooting = \_ -> [IK.knightAtropos],
            turnMelee = \_ _ -> [IK.knightAtropos]
          },

          chargeFilter (chargeRoll RerollChargeOneDie 10) $
          eraseTurn emptyTurn {
            turnName = "3 conqueror dragoons",
            turnMelee = \_ _ ->
              with AdMech.conquerorDoctrinaImperative $
                replicate 3 AdMech.taserLanceDragoon
          }
        ],

        -- AdMech
        eraseTurn emptyTurn {
          turnName = "5 EV cawl WoM dakkabots + 3 rr1 plasmaphrons",
          turnShooting = \_ ->
            with AdMech.eliminationVolley $ concat [
              within AdMech.cawlAura $
                with AdMech.wrathOfMars $
                  AdMech.dakkabots 5 AdMech.Protector,

              within AdMech.dominusAura $
                replicate 3 AdMech.plasmaKataphronOvercharge
            ]
        },

        eraseTurn emptyTurn {
          turnName = "6 EV mindlock rr1 ryza plasmaphrons",
          turnShooting = \_ ->
            within AdMech.dominusAura $
              with (AdMech.plasmaSpecialists . AdMech.neosphericMindlock . AdMech.eliminationVolley) $
                replicate 6 AdMech.plasmaKataphronOvercharge
        }
      ]

    bigKnights =
      [
        chargeFilter (do { adv <- d6; chargeRoll RerollChargeOneDie (24 - 12-adv-2 - 2) }) $
        eraseTurn emptyTurn {
          turnName = "landstrider gallant paragon gt.",
          turnMelee = \_ _ -> [IK.knightGallant IK.paragonGauntlet]
        },

        chargeFilter (do { adv <- d6; chargeRoll RerollChargeOneDie (24 - 12-adv-2 - 2) }) $
        eraseTurn emptyTurn {
          turnName = "landstrider gallant ravager",
          turnMelee = \_ _ -> [IK.knightGallant IK.ravager]
        },

        chargeFilter (do { adv <- d6; chargeRoll RerollChargeOneDie (24 - 12-adv-2 - 2) }) $
        eraseTurn emptyTurn {
          turnName = "landstrider c.a. gallant stomps",
          turnMelee = \_ _ ->
            with (IK.controlledAggressionChaos . IK.stomping) $
              [IK.knightGallant IK.reaperChainsword]
        },

        eraseTurn emptyTurn {
          turnName = "atropos (shooting+lascutter)",
          turnShooting = \_ -> [IK.knightAtropos],
          turnMelee = \_ _ -> [IK.knightAtropos]
        },

        eraseTurn emptyTurn {
          turnName = "knight styrix (shooting+claw)",
          turnShooting = \_ -> [IK.knightStyrix [twin IK.radCleanser]],
          turnMelee = \_ _ -> [IK.knightStyrix [twin IK.radCleanser]]
        }
      ]


plagueMarineTests :: IO ()
plagueMarineTests = do
    R.analysisToFile "/tmp/plague-distrib.png"
      [ AnalysisConfig ByTarget  ProbKill                         plagueMarineCounters [(5, DG.plagueMarineModel)]
      , AnalysisConfig ByTarget (SlainModels RevDistributionPlot) plagueMarineCounters [DG.plagueMarineModel]
      ]

    return ()
  where
    plagueMarineCounters =
      [
        eraseTurn emptyTurn {
          turnName = "malleus prometheus",
          turnShooting = \_ ->
            with (em_model %~ Inq.quarryBonus Inq.OrdoMalleus) $
              [Inq.landraiderPrometheus]
        },
        eraseTurn emptyTurn {
          turnName = "psybolt SR w/Draigo",
          turnShooting = \_ ->
            within GK.draigoAura $ with (rapidFiring . GK.psyboltAmmo) $
              [Marines.stormravenWith [hurricaneBolter, hurricaneBolter, twin heavyBolter]]
        },
        eraseTurn emptyTurn {
          turnName = "psybolt 10x SS w/Draigo",
          turnShooting = \_ ->
            within GK.draigoAura $ with (rapidFiring . GK.psyboltAmmo) $
              GK.strikeSquad 10 GK.twoFalchions
        },
        eraseTurn emptyTurn {
          turnName = "2x stormhawk AssCan+HB w/Draigo",
          turnShooting = \_ ->
            within GK.draigoAura $ with moving $
              two [Marines.stormhawkWith (two [assaultCannon] ++ two [heavyBolter])]
        },
        eraseTurn emptyTurn {
          turnName = "2 helverins",
          turnShooting = \_ -> two [IK.armigerHelverin]
        },
        eraseTurn emptyTurn {
          turnName = "3 protector autocannon ironstriders",
          turnShooting = \_ ->
            with AdMech.protectorDoctrinaImperative $
              replicate 3 AdMech.autocannonIronstrider
        },
        eraseTurn emptyTurn {
          turnName = "2 protector dakkabots RR1",
          turnShooting = \_ ->
            within AdMech.dominusAura $
              AdMech.dakkabots 2 AdMech.Protector
        }
      ]


arcWeaponryTests :: IO ()
arcWeaponryTests = do
    R.analysisToFile "/tmp/arc-weaponry.png"
      [ AnalysisConfig ByTarget WoundingSummary                    (rangedWeaponry ++ meleeBreachers) vehicles
      , AnalysisConfig ByTarget (NumWoundsMax RevDistributionPlot) rangedWeaponry vehicles
      , AnalysisConfig ByTarget (NumWoundsMax RevDistributionPlot) meleeBreachers vehicles
      ]

    return ()
  where
    vehicles =
        [ Marines.predator
        , Marines.stormraven
        ]

    rangedWeaponry =
      [
        eraseTurn emptyTurn {
          turnName = "6 mindlock rr1 arc kataphrons",

          turnShooting = \_ ->
            within AdMech.dominusAura $
              with AdMech.neosphericMindlock $
                replicate 6 (AdMech.arcKataphron Vehicle)
        },

        eraseTurn emptyTurn {
          turnName = "4 mindlock rr1 torsion kataphrons",

          turnShooting = \_ ->
            within AdMech.dominusAura $
              with AdMech.neosphericMindlock $
                replicate 4 AdMech.torsionKataphron
        },

        eraseTurn emptyTurn {
          turnName = "6 mindlock rr1 xi-lexum arc kataphrons",

          turnShooting = \_ ->
            within AdMech.dominusAura $
              with (AdMech.eyeOfXiLexum . AdMech.neosphericMindlock) $
                replicate 6 (AdMech.arcKataphron Vehicle)
        },

        eraseTurn emptyTurn {
          turnName = "4 mindlock rr1 xi-lexum torsion kataphrons",

          turnShooting = \_ ->
            within AdMech.dominusAura $
              with (AdMech.eyeOfXiLexum . AdMech.neosphericMindlock) $
                replicate 4 AdMech.torsionKataphron
        },

        eraseTurn emptyTurn {
          turnName = "6 RF xi-lexum arc rangers",

          turnShooting = \_ ->
            within AdMech.dominusAura $
              with (rapidFiring . AdMech.eyeOfXiLexum) $
                replicate 6 (AdMech.rangerWith (AdMech.arcRifle Vehicle))
        }
      ]

    meleeBreachers =
      [
        eraseTurn emptyTurn {
          turnName = "6 hermeticon arc kataphrons",

          turnMelee = \_ _ ->
            within AdMech.primeHermeticonAura $
              replicate 6 (AdMech.arcKataphron Vehicle)
        },

        eraseTurn emptyTurn {
          turnName = "6 hermeticon machine might arc kataphrons",

          turnMelee = \_ _ ->
            within AdMech.primeHermeticonAura $
              with AdMech.invocationOfMachineMight $
                replicate 6 (AdMech.arcKataphron Vehicle)
        },

        eraseTurn emptyTurn {
          turnName = "6 hermeticon hydraulic kataphrons",

          turnMelee = \_ _ ->
            within AdMech.primeHermeticonAura $
              with AdMech.invocationOfMachineMight $
                replicate 6 AdMech.torsionKataphron
        }
      ]
