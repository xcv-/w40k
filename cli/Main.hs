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


stormbolters5 = eraseTurn emptyTurn
  & turnName .~ "5 rapid-fire stormbolters (psybolt)"
  & turnShooting_ .~ do
      with [GK.psyboltAmmo, rapidFiring] $ GK.terminatorSquad 5 GK.halberd

stormbolters10 = eraseTurn emptyTurn
  & turnName .~ "10 rapid-fire stormbolters (psybolt)"
  & turnShooting_ .~ do
      with [GK.psyboltAmmo, rapidFiring] $ GK.terminatorSquad 10 GK.halberd

psilencers = eraseTurn emptyTurn
  & turnName .~ " psilencer purgators"
  & turnShooting_ .~ do
      with [] $ GK.purgatorSquad GK.psilencer

psycannons = eraseTurn emptyTurn
  & turnName .~ " psycannon purgators"
  & turnShooting_ .~ do
      with [] $ GK.purgatorSquad GK.psycannon

gmndk = eraseTurn emptyTurn
  & turnName .~ "gmndk"
  & turnShooting_ .~ do
      [GK.gmndkWith [GK.gatlingPsilencer, GK.heavyPsycannon] GK.greatsword]


onslaughtPsilencers = psilencers & turnShooting_ %~ with [GK.psyOnslaught] & turnName <>~ " (onslaught)"
onslaughtPsycannons = psycannons & turnShooting_ %~ with [GK.psyOnslaught] & turnName <>~ " (onslaught)"


baseList = [stormbolters5, stormbolters10, psilencers, onslaughtPsilencers, psycannons, onslaughtPsycannons]
inCover = stack [model_save -~ 1, model_name <>~ " (cover)"]

invocationList              = map (turnAttacks_  %~ with [GK.invocationOfFocus]) baseList
tideList                    = map (turnShooting_ %~ with [GK.tideOfConvergence]) baseList
invocationTideList          = map (turnAttacks_  %~ with [GK.invocationOfFocus]) tideList
invocationOnslaughtTideList = map (turnShooting_ %~ with [GK.psyOnslaught])      invocationTideList


allCombinations bdtb fn targets =
    [ AnalysisConfigGroup "Base list"
        [ AnalysisConfig ByTarget fn
            (f baseList)
            targets
        ]
    , AnalysisConfigGroup "Base list with ToC"
        [ AnalysisConfig ByTarget fn
            (f tideList)
            targets
        ]
    , AnalysisConfigGroup "Base list with IoF"
        [ AnalysisConfig ByTarget fn
            (f invocationList)
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
                  & turnShooting_ %~ with [moving]
                  & turnName %~ ("moving " <>)
              , gmndk

              , gmndk
                  & turnShooting_ %~ with [moving]
                  & turnName %~ ("moving " <>)
                  & turnShooting_ %~ with [GK.psyOnslaught]
                  & turnName <>~ " (onslaught)"
              , gmndk
                  & turnShooting_ %~ with [GK.psyOnslaught]
                  & turnName <>~ " (onslaught)"
              ])
            targets
        ]
    ]
  where
    f | bdtb      = map (turnAttacks_ %~ with [GK.bringDownTheBeast])
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

