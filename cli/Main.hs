module Main where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import W40K.Core.Prob
import W40K.Core.Chart
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


main = do
    let chaosPrimarchs =
          [ DG.mortarion       DG.silenceReaping
          , DG.buffedMortarion DG.silenceReaping
          , TSons.magnus
          , TSons.buffedMagnus
          ]

    let rangedAttackers = setCombatType Ranged
          [ ("onslaught GMNDK + SR AT + SH las",
              with (GK.psyOnslaughtAmmo . moving)
                [GK.gmndk]
              ++
              (with moving . within GK.draigoAura)
                [ Marines.stormravenWith [lascannon, Marines.stormstrike, Marines.stormstrike]
                , Marines.stormhawkWith [Marines.lastalon] & em_model %~ Marines.interceptorJet
                , Marines.stormhawkWith [Marines.lastalon] & em_model %~ Marines.interceptorJet
                ])

          , ("knight styrix",
              with (em_model %~ IK.firstKnight)
                [IK.knightStyrix []])

          , ("headman's mark paladin + stormspear",
              [IK.headman'sMark (IK.knightPaladin [IK.stormspearRocketPod] IK.reaperChainsword)])

          , ("helverin + AA helverin",
              [ IK.armigerHelverin
              , IK.armigerHelverin & em_model.model_rng_mods.mod_rrtohit .~ RerollFailed
              ])

          , ("atropos",
              [IK.knightAtropos])

          , ("3 protector autocannon ironstriders",
              with AdMech.protectorDoctrinaImperative $
                replicate 3 AdMech.autocannonIronstrider)

          , ("3 protector lascannon ironstriders",
              with AdMech.protectorDoctrinaImperative $
                replicate 3 AdMech.lascannonIronstrider)

          , ("2 rr1 neutronagers",
              within AdMech.dominusAura $
                replicate 2 AdMech.neutronOnager)
          ]

    let meleeAttackers = setCombatType Melee
          [ ("gallant paragon gt.",
              [IK.knightGallant IK.paragonGauntlet])

          , ("gallant ravager",
              [IK.knightGallant IK.ravager])

          , ("c.a. gallant stomps",
              with (IK.controlledAggressionChaos . IK.stomping) $
                [IK.knightGallant IK.reaperChainsword])

          , ("atropos",
              [IK.knightAtropos])

          , ("GMNDK",
              [GK.gmndk & em_model.model_cc_mods.mod_rrtowound <>~ RerollFailed])

          , ("3 conqueror dragoons",
              with AdMech.conquerorDoctrinaImperative $
                replicate 3 AdMech.taserLanceDragoon)
          ]

    analysisToSvgFile "/tmp/primarchs-avg.svg"
      [ analysisConfig' ByTarget AverageWounds chaosPrimarchs rangedAttackers
      , analysisConfig' ByTarget AverageWounds chaosPrimarchs meleeAttackers
      ]

    analysisToSvgFile "/tmp/primarchs-distrib.svg"
      [ analysisConfig' ByTarget (NumWoundsMax RevDistributionPlot) chaosPrimarchs rangedAttackers
      , analysisConfig' ByTarget (NumWoundsMax RevDistributionPlot) chaosPrimarchs meleeAttackers
      ]

    analysisToSvgFile "/tmp/plague-distrib.svg"
      [ analysisConfig' ByTarget (SlainModels RevDistributionPlot) [DG.plagueMarineModel] $
          setCombatType Ranged
            [ ("malleus prometheus",
                with (em_model %~ Inq.quarryBonus Inq.OrdoMalleus) $
                  [Inq.landraiderPrometheus])

            , ("psybolt SR w/Draigo",
                within GK.draigoAura $ with (rapidFiring . GK.psyboltAmmo) $
                  [Marines.stormravenWith [hurricaneBolter, hurricaneBolter, twin heavyBolter]])

            , ("psybolt 10x SS w/Draigo",
                within GK.draigoAura $ with (rapidFiring . GK.psyboltAmmo) $
                  GK.strikeSquad 10 GK.twoFalchions)

            , ("2x stormhawk AssCan+HB w/Draigo",
                within GK.draigoAura $ with moving $
                  two [Marines.stormhawkWith (two [assaultCannon] ++ two [heavyBolter])])

            , ("2 helverins",
                two [IK.armigerHelverin])

            , ("3 protector autocannon ironstriders",
                with AdMech.protectorDoctrinaImperative $
                  replicate 3 AdMech.autocannonIronstrider)

            , ("2 protector dakkabots RR1",
                within AdMech.dominusAura $
                  AdMech.dakkabots 2 AdMech.Protector)
            ]
      ]
