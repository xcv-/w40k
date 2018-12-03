module Main where

import Prelude hiding (Functor(..), Monad(..))

import Control.Lens

import W40K.Core.Prob
import W40K.Core.Chart
import W40K.Core.Mechanics
import W40K.Core.Psychic

import W40K.Data.Common
import qualified W40K.Data.AdMech       as AdMech
import qualified W40K.Data.Assassins    as Assassins
import qualified W40K.Data.Chaos        as Chaos
import qualified W40K.Data.Eldar        as Eldar
import qualified W40K.Data.GreyKnights  as GK
import qualified W40K.Data.Marines      as Marines
import qualified W40K.Data.Necrons      as Necrons
import qualified W40K.Data.Tau          as Tau
import qualified W40K.Data.ThousandSons as TSons
import qualified W40K.Data.Tyranids     as Tyranids


main = do

    analysisToSvgFile "/tmp/test-foldwounds.svg"
      [ analysisConfig' ByTarget (SlainModels RevDistributionPlot) [AdMech.infiltratorModel] $
          setCombatType Ranged
              [ ("MSU psilencer purgation squad",
                  GK.purgatorSquad GK.psilencer)
              , ("MSU psilencer purgation squad (moving)",
                  with moving
                       (GK.purgatorSquad GK.psilencer))
              , ("MSU psilencer purgation squad (psyonslaught)",
                  with GK.psyOnslaughtAmmo
                       (GK.purgatorSquad GK.psilencer))
              , ("MSU psilencer purgation squad (moving) (psyonslaught)",
                  with (moving . GK.psyOnslaughtAmmo)
                       (GK.purgatorSquad GK.psilencer))
              ]
      , analysisConfig' ByTarget (SlainModels RevDistributionPlot) [AdMech.infiltratorModel] $
          setCombatType Melee
              [ ("MSU strike squad",
                  GK.strikeSquad 5 GK.twoFalchions)
              , ("MSU terminator squad",
                  GK.terminatorSquad 5 GK.twoFalchions)
              , ("MSU HH paladins",
                  with GK.hammerhand
                       (GK.paladinSquad 3 GK.halberd))
              ]
      ]

    analysisToSvgFile "/tmp/test-numwoundsmax.svg"
      [ analysisConfig' ByTarget (NumWoundsMax RevDistributionPlot) [AdMech.radSaturation TSons.daemonPrinceModel, TSons.daemonPrinceModel] $
          setCombatType Melee
            [ ("5 ryza conqueror + machine might taser infiltrators",
                with (AdMech.ryzaDogma . AdMech.conquerorDoctrinaImperative . AdMech.invocationOfMachineMight)
                     (AdMech.taserGoadInfiltrators 5))
            , ("5 ryza conqueror + remorseless fist taser infiltrators",
                with (AdMech.ryzaDogma . AdMech.conquerorDoctrinaImperative . AdMech.chantOfTheRemorselessFist)
                     (AdMech.taserGoadInfiltrators 5))
            , ("5 ryza conqueror + machine might blades ruststakers",
                with (AdMech.ryzaDogma . AdMech.conquerorDoctrinaImperative . AdMech.invocationOfMachineMight)
                     (AdMech.bladesRuststalkers 5))
            , ("5 ryza conqueror + remorseless fist blades ruststakers",
                with (AdMech.ryzaDogma . AdMech.conquerorDoctrinaImperative . AdMech.chantOfTheRemorselessFist)
                     (AdMech.bladesRuststalkers 5))
            , ("5 ryza conqueror + machine might razor+claw ruststakers",
                with (AdMech.ryzaDogma . AdMech.conquerorDoctrinaImperative . AdMech.invocationOfMachineMight)
                     (AdMech.razorClawRuststalkers 5))
            , ("5 ryza conqueror + remorseless fist razor+claw ruststakers",
                with (AdMech.ryzaDogma . AdMech.conquerorDoctrinaImperative . AdMech.chantOfTheRemorselessFist)
                     (AdMech.razorClawRuststalkers 5))
            ]
      ]
