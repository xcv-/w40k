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
    ikKillingTests


plasmaphrons =
  eraseTurn emptyTurn {
    turnName = "EV plasmaphrons",

    turnShooting = \_ ->
      within AdMech.dominusAura $
        with AdMech.eliminationVolley $
          replicate 4 $ set em_rw [AdMech.plasmaCulverinOvercharge] AdMech.plasmaKataphronOvercharge
  }

dakkabots =
  eraseTurn emptyTurn {
    turnName = "EV dakkabots",

    turnShooting = \_ ->
      within AdMech.dominusAura $
        with AdMech.eliminationVolley $
          AdMech.dakkabots 3 AdMech.Protector
  }

breachers n =
  eraseTurn emptyTurn {
    turnName = show n ++ " breachers",
    turnShooting = \_ ->
      replicate n $ AdMech.arcKataphron Vehicle
  }

icarusCrawlers fly =
  eraseTurn emptyTurn {
    turnName = "icarus",

    turnShooting = \_ ->
      replicate 2 (AdMech.icarusOnager fly)
  }

dragoons =
  eraseTurn emptyTurn {
    turnName = "dragoons",

    turnMelee = \_ _ ->
      with AdMech.conquerorDoctrinaImperative $
        replicate 3 $ AdMech.taserLanceDragoon
  }


ikKillingTests :: IO ()
ikKillingTests = do
   R.analysisToFile "/tmp/ik-killing.png"
     [ AnalysisConfig ByTarget ProbKillOne                         [shootingOnly (mconcat list)]  knights
     , AnalysisConfig ByTarget WoundingSummary                     [shootingOnly (mconcat list)]  knights
     , AnalysisConfig ByTarget (NumWoundsMax RevDistributionPlot)  list                           knights
     ]

   return ()
  where
    shootingOnly = mapGenericTurn $ set fightPhase [] . over renaming (<> " (shooting)")
    mindlock = mapGenericTurn $ over shootingPhase (with AdMech.neosphericMindlock) . over renaming (<> " (mindlock)")

    list =
      [ mindlock plasmaphrons
      , dakkabots
      , breachers 4
      , breachers 4
      , dragoons
      ]

    knights =
      [ IK.questorisModel
      , IK.rotateIonShields IK.questorisModel
      ]

    chaosPrimarchs =
      [ DG.mortarion       DG.silenceReaping
      , DG.buffedMortarion DG.silenceReaping
      , TSons.magnus
      , TSons.buffedMagnus
      ]


