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

--import GreyKnightTests


main = do
    analysisToSvgFile "/tmp/output.svg"
        [ analysisConfig' ByTarget (NumWounds RevDistributionPlot) [TSons.magnus^.em_model, TSons.buffedMagnus^.em_model] $
            setCombatType Melee
              [ ("6 conqueror dragoons",                           with AdMech.conquerorDoctrinaImperative $
                                                                      replicate 6 AdMech.taserLanceDragoon)
              , ("6 conqueror + remorseless fist dragoons",        with (AdMech.conquerorDoctrinaImperative . AdMech.chantOfTheRemorselessFist) $
                                                                      replicate 6 AdMech.taserLanceDragoon)
              , ("6 conqueror + remorseless fist, ryza dragoons",  with (AdMech.conquerorDoctrinaImperative . AdMech.chantOfTheRemorselessFist . AdMech.ryzaDogma) $
                                                                      replicate 6 AdMech.taserLanceDragoon)
              ]
            ++
            setCombatType Ranged
             [ ("3 mars dakkabots + WoM",         within AdMech.cawlAura $ with AdMech.wrathOfMars $ AdMech.dakkabots 3 AdMech.Protector)
             , ("4 mars dakkabots + WoM",         within AdMech.cawlAura $ with AdMech.wrathOfMars $ AdMech.dakkabots 4 AdMech.Protector)
             , ("3 mars dakkabots + WoM + EV",    within AdMech.cawlAura $ with (AdMech.eliminationVolley . AdMech.wrathOfMars) $ AdMech.dakkabots 3 AdMech.Protector)
             , ("4 mars dakkabots + WoM + EV",    within AdMech.cawlAura $ with (AdMech.eliminationVolley . AdMech.wrathOfMars) $ AdMech.dakkabots 4 AdMech.Protector)
             ]
        ]
