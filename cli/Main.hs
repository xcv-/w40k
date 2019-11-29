{-# language RebindableSyntax #-}
module Main where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens hiding ((<.>))

import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))

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
import qualified W40K.Data.Custodes        as Custodes
import qualified W40K.Data.Daemons         as CD
import qualified W40K.Data.DeathGuard      as DG
import qualified W40K.Data.Eldar           as Eldar
import qualified W40K.Data.GreyKnights     as GK
import qualified W40K.Data.ImperialKnights as IK
import qualified W40K.Data.ImperialGuard   as IG
import qualified W40K.Data.Inquisition     as Inq
import qualified W40K.Data.Marines         as SM
import qualified W40K.Data.Necrons         as Necrons
import qualified W40K.Data.Tau             as Tau
import qualified W40K.Data.ThousandSons    as TSons
import qualified W40K.Data.Tyranids        as Tyranids


baseDir :: FilePath
baseDir = "/tmp/analysis"


main :: IO ()
main = do
  createDirectoryIfMissing False baseDir

  R.withEmbeddedR $ do
    testHordes
    testSuperheavy
    testToughness8
    testPrimarchs
    testFlyVehicles
    testEldarFlyers
    testFlyerUnits


plasmaphrons =
  eraseTurn emptyTurn {
    turnName = "4 EV plasmaphrons",

    turnShooting = \_ ->
      within AdMech.dominusAura $
        with AdMech.eliminationVolley $
          replicate 4 $ set em_rw [AdMech.plasmaCulverinOvercharge] AdMech.plasmaKataphronOvercharge
  }

plasmaphronsNoOC = plasmaphrons
  & mapGenericTurn (shootingPhase.mapped.em_rw .~ [AdMech.plasmaCulverin])
  & mapGenericTurn (renaming <>~ " (no OC)")

dakkabots =
  eraseTurn emptyTurn {
    turnName = "3 EV dakkabots",

    turnShooting = \_ ->
      within AdMech.dominusAura $
        with AdMech.eliminationVolley $
          AdMech.dakkabots 3 AdMech.Protector
  }

breachers kw =
  eraseTurn emptyTurn {
    turnName = "7 breachers",
    turnShooting = \_ ->
      within AdMech.dominusAura $
        replicate 7 $ AdMech.arcKataphron kw
  }

allIcarusCrawlers :: Bool -> GenericTurn
allIcarusCrawlers = icarusCrawlers 3

icarusCrawlers n fly =
  eraseTurn emptyTurn {
    turnName = show n ++ " icarus",

    turnShooting = \_ ->
      within AdMech.dominusAura $
        replicate n (AdMech.icarusOnager fly)
  }

dragoons =
  eraseTurn emptyTurn {
    turnName = "dragoons",

    turnMelee = \_ _ ->
      with AdMech.conquerorDoctrinaImperative $
        replicate 3 $ AdMech.taserLanceDragoon
  }

shootingOnly = mapGenericTurn $
  set fightPhase []
  . over renaming (<> " (shooting)")

mindlock = mapGenericTurn $
  over shootingPhase (with AdMech.neosphericMindlock)
  . over renaming (<> " (mindlock)")

daedalosus = mapGenericTurn $
  over shootingPhase (with AdMech.daedalosusTarget)
  . over renaming (<> " (Dr.D)")


defaultTests list targets =
    [ AnalysisConfig ByTarget ProbKillOne                         [shootingOnly (mconcat list)]  targets
    , AnalysisConfig ByTarget WoundingSummary                     [shootingOnly (mconcat list)]  targets
    , AnalysisConfig ByTarget (NumWoundsMax RevDistributionPlot)  list                           targets
    ]

runTests name configs =
    R.analysisToFile (baseDir </> name <.> "png") configs >> return ()

runDefaultTests name list targets =
    runTests name (defaultTests list targets)



testHordes =
    runTests "hordes"
      [ AnalysisConfig ByTarget   (SlainModels RevDistributionPlot) list                                     targets
      , AnalysisConfig ByAttacker (SlainModels RevDistributionPlot) [shootingOnly (mconcat list)]            targets
      , AnalysisConfig ByTarget   (SlainModels RevDistributionPlot) (map daedalosus list)                    targets
      , AnalysisConfig ByAttacker (SlainModels RevDistributionPlot) [shootingOnly (foldMap daedalosus list)] targets
      ]
  where
    list =
      [ dakkabots
      , mindlock plasmaphrons
      , dragoons
      , allIcarusCrawlers False
      ]

    targets =
      [ CD.plagueBearer
      , CD.warpSurge $ CD.cloudOfFlies $ CD.plagueBearer
      , CD.miasmaOfPestilence $ CD.cloudOfFlies $ CD.plagueBearer

      , SM.fleshIsWeak $ SM.primaris

      , stormShield meq & model_name .~ "DW veteran w/SS"
      ]

testSuperheavy =
    runTests "super-heavy"
      [ AnalysisConfig ByTarget ProbKillOne                         [shootingOnly (mconcat list)]  targets
      , AnalysisConfig ByTarget WoundingSummary                     [shootingOnly (mconcat list)]  targets
      , AnalysisConfig ByTarget ProbKillOne                         [mconcat list]                 targets
      , AnalysisConfig ByTarget WoundingSummary                     list                           targets
      , AnalysisConfig ByTarget (NumWoundsMax RevDistributionPlot)  list                           targets
      ]
  where
    list =
      [ breachers Vehicle & daedalosus
      , dakkabots
      , mindlock plasmaphrons
      , dragoons
      ]

    targets =
      [ IK.questorisModel
      , IK.rotateIonShields IK.questorisModel

      , IG.baneblade
      ]


testToughness8 =
    runTests "toughness8"
      [ AnalysisConfig ByTarget ProbKillOne                         [shootingOnly (mconcat list)]  targets
      , AnalysisConfig ByTarget WoundingSummary                     [shootingOnly (mconcat list)]  targets
      , AnalysisConfig ByTarget ProbKillOne                         [mconcat list]                 targets
      , AnalysisConfig ByTarget WoundingSummary                     list                           targets
      , AnalysisConfig ByTarget (NumWoundsMax RevDistributionPlot)  list                           targets
      ]
  where
    list =
      [ breachers Vehicle
      , dakkabots
      , allIcarusCrawlers False
      , mindlock plasmaphrons
      , dragoons
      ]

    targets =
      [ IG.lemanRuss
      , SM.leviathanDreadnought
      , Custodes.telemon
      ]

testPrimarchs = runTests "primarchs"
    [ AnalysisConfig ByTarget ProbKillOne                         [mconcat list]                targets
    , AnalysisConfig ByTarget WoundingSummary                     [mconcat list]                targets
    , AnalysisConfig ByTarget ProbKillOne                         [shootingOnly (mconcat list)] targets
    , AnalysisConfig ByTarget WoundingSummary                     [shootingOnly (mconcat list)] targets
    , AnalysisConfig ByTarget WoundingSummary                     list                          targets
    , AnalysisConfig ByTarget (NumWoundsMax RevDistributionPlot)  list                          targets
    ]
  where
    list =
      [ mindlock plasmaphrons
      , dakkabots
      , allIcarusCrawlers True
      , dragoons
      ]

    targets =
      [ DG.mortarion       DG.silenceReaping
      , DG.buffedMortarion DG.silenceReaping

      , TSons.magnus
      , TSons.buffedMagnus
      ]


testFlyVehicles =
    runTests "fly-vehicles" $
      concat $ zipWith (\a b -> [a,b])
        (defaultTests (plasmaphronsNoOC : list) waveSerpents)
        (defaultTests (plasmaphrons : list)     custodesTanks)

  where
    list =
      [ dakkabots
      , breachers Vehicle
      , allIcarusCrawlers True
      , dragoons
      ]

    waveSerpents =
      [ Eldar.ulthwe Eldar.waveSerpent
      ]

    custodesTanks =
      [ Custodes.pallas
      , Custodes.calladius
      , Custodes.vexillaMagnifica Custodes.pallas
      , Custodes.vexillaMagnifica Custodes.calladius
      ]

testEldarFlyers = runDefaultTests "eldar-flyers" list targets
  where
    list =
      [ plasmaphronsNoOC
      , dakkabots
      , breachers Vehicle
      , allIcarusCrawlers True
      ]

    targets =
      [ Eldar.inuredToSuffering $ Eldar.razorwing
      , Eldar.lightningReflexes $ Eldar.inuredToSuffering $ Eldar.razorwing

      , Eldar.alaitoc $ Eldar.crimsonHunter
      , Eldar.lightningReflexes $ Eldar.alaitoc $ Eldar.crimsonHunter
      ]


testFlyerUnits =
    runTests "flyer-units" $
      [ AnalysisConfig ByTarget   WoundingSummary                   list targets
      , AnalysisConfig ByTarget   (SlainModels RevDistributionPlot) list targets
      , AnalysisConfig ByAttacker (SlainModels RevDistributionPlot) [shootingOnly (mconcat list)] targets
      ]
  where
    list =
      [ allIcarusCrawlers True
      , mindlock plasmaphrons
      , dakkabots
      , breachers Biker
      , dragoons
      ]

    targets =
      [ Custodes.vertusPraetor
      , Custodes.vexillaMagnifica Custodes.vertusPraetor
      , Custodes.auricAquilas $ Custodes.superiorCreation $ Custodes.dawneagleCaptain
      , Custodes.auricAquilas $ Custodes.radiantMantle $ Custodes.dawneagleCaptain
      , Custodes.vexillaMagnifica $ Custodes.auricAquilas $ Custodes.radiantMantle $ Custodes.dawneagleCaptain

      , Eldar.prophetsOfFlesh $ Eldar.masterOfPain $ Eldar.talos

      , Eldar.skyweaver
      , Eldar.prismaticBlur $ Eldar.lightningReflexes $ Eldar.skyweaver
      ]
