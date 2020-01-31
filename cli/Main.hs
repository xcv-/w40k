{-# language RebindableSyntax #-}
module Main where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens hiding ((<.>))

import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))

import W40K.Core.ConstrMonad
import W40K.Core.Prob
import W40K.Core.Chart (AnalysisConfig(..), AnalysisConfigGroup(..), AnalysisOrder(..), ProbPlotType(..), AnalysisFn(..))
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
import qualified W40K.Data.ThousandSons    as TS
import qualified W40K.Data.Tyranids        as Tyranids


baseDir :: FilePath
baseDir = "/tmp/analysis"


main :: IO ()
main = do
  createDirectoryIfMissing False baseDir

  R.withEmbeddedR $ do
    testMEQ
    testRubrics
    testToughness7
    testToughness7_bringDownTheBeast
    testToughness8
    testToughness8_bringDownTheBeast


stormbolters = eraseTurn emptyTurn
  & turnName .~ "5 rapid-fire stormbolters"
  & turnShooting .~ do
      with rapidFiring $ GK.terminatorSquad 5 GK.halberd

psilencers = eraseTurn emptyTurn
  & turnName .~ "moving psilencer purgators"
  & turnShooting .~ do
      with moving $ GK.purgatorSquad GK.psilencer

psycannons = eraseTurn emptyTurn
  & turnName .~ "moving psycannon purgators"
  & turnShooting .~ do
      with moving $ GK.purgatorSquad GK.psycannon

gmndk = eraseTurn emptyTurn
  & turnName .~ "gmndk"
  & turnShooting .~ do
      [GK.gmndkWith [GK.gatlingPsilencer, GK.heavyPsycannon] GK.greatsword]


psyboltStormbolters = stormbolters & turnShooting %~ with GK.psyboltAmmo  & turnName <>~ " (psybolt)"
onslaughtPsilencers = psilencers   & turnShooting %~ with GK.psyOnslaught & turnName <>~ " (onslaught)"
onslaughtPsycannons = psycannons   & turnShooting %~ with GK.psyOnslaught & turnName <>~ " (onslaught)"


baseList = [stormbolters, psyboltStormbolters, psilencers, onslaughtPsilencers, psycannons, onslaughtPsycannons]
inCover m = m & model_save -~ 1 & model_name <>~ " (cover)"

invocationList              = map (turnShooting %~ with GK.invocationOfFocus) baseList
tideList                    = map (turnShooting %~ with GK.tideOfConvergence) baseList
invocationTideList          = map (turnShooting %~ with GK.invocationOfFocus) tideList
invocationOnslaughtTideList = map (turnShooting %~ with GK.psyOnslaught)      invocationTideList


allCombinations bdtb fn targets =
    [ AnalysisConfigGroup "Base list"
        [ AnalysisConfig ByTarget fn
            (f baseList)
            targets
        ]
    , AnalysisConfigGroup "Base list with IoF"
        [ AnalysisConfig ByTarget fn
            (f invocationList)
            targets
        ]
    , AnalysisConfigGroup "Base list with ToC"
        [ AnalysisConfig ByTarget fn
            (f tideList)
            targets
        ]
    , AnalysisConfigGroup "Base list with IoF+ToC"
        [ AnalysisConfig ByTarget fn
            (f invocationTideList)
            targets
        ]
    , AnalysisConfigGroup "GMNDK"
        [ AnalysisConfig ByTarget WoundingSummary
            (f
              [ gmndk
                  & turnShooting %~ with moving
                  & turnName %~ ("moving " <>)
              , gmndk

              , gmndk
                  & turnShooting %~ with moving
                  & turnName %~ ("moving " <>)
                  & turnShooting %~ with GK.psyOnslaught
                  & turnName <>~ " (onslaught)"
              , gmndk
                  & turnShooting %~ with GK.psyOnslaught
                  & turnName <>~ " (onslaught)"
              ])
            targets
        ]
    ]
  where
    f | bdtb      = map (turnAttacks %~ with GK.bringDownTheBeast)
      | otherwise = id


testMEQ =
    runTests "Marines" $
      allCombinations False SlainSummary targets
  where
    targets =
      [ meq
      , SM.primaris
      , inCover meq
      , inCover SM.primaris
      ]

testRubrics =
    runTests "Rubrics" $
      allCombinations False SlainSummary targets
  where
    targets =
      [ TS.rubricModel
      , inCover TS.rubricModel
      , TS.scarabOccultModel
      , inCover TS.scarabOccultModel
      ]

testToughness7 =
    runTests "Toughness 7" $
      allCombinations False WoundingSummary targets
  where
    targets =
      [ rhino
      , Eldar.waveSerpent
      , Tau.riptide
      , Tau.novaShield Tau.riptide
      ]

testToughness7_bringDownTheBeast =
    runTests "Toughness 7 (Bring Down the Beast)" $
      allCombinations True WoundingSummary targets
  where
    targets =
      [ rhino
      , Eldar.waveSerpent
      ]

testToughness8 =
    runTests "Toughness 8" $
      allCombinations False WoundingSummary targets
  where
    targets =
      [ SM.leviathanDreadnought
      , IK.questorisModel
      , IK.rotateIonShields IK.questorisModel
      ]

testToughness8_bringDownTheBeast =
    runTests "Toughness 8  (Bring Down the Beast)" $
      allCombinations True WoundingSummary targets
  where
    targets =
      [ SM.leviathanDreadnought
      , IK.questorisModel
      , IK.rotateIonShields IK.questorisModel
      ]


runTests name configs =
    R.analysisToFile (baseDir </> name <.> "png") name configs >> return ()

