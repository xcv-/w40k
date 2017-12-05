{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module W40K.Core.Chart where

import Data.Coerce (coerce)
import Data.Constraint (Dict(..))
import Data.Reflection (Given(..), give)

import Control.Arrow ((***))
import Control.Monad (forM_, (<=<))
import Control.Monad.State (evalState, evalStateT, execStateT)
import Control.DeepSeq (NFData, force)

import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.SVG         (renderSVG)
import Diagrams.Backend.CmdLine     (mainWith)
import Diagrams.Backend.SVG.CmdLine (SVG)

import qualified Graphics.Rendering.Chart                  as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as ChartD

import Debug.Trace

import W40K.Core.Prob (Event(..), Prob, QQ, events, distribution, revDistribution)
import W40K.Core.Mechanics


moreColors :: [AlphaColour Double]
moreColors = cycle $ map opaque
  [black, blue, brown, chocolate, cyan, darkgreen, fuchsia, gold, gray,
   greenyellow, lightpink, olive, orange, red, yellow]

modNamedEqUnit :: String -> Modifier -> [NamedEqUnit] -> [NamedEqUnit]
modNamedEqUnit modName mod = map $ \(name, ct, squad) ->
    (name ++ " (" ++ modName ++ ")", ct, mod squad)

setCombatType :: CombatType -> [(String, [EquippedModel])] -> [NamedEqUnit]
setCombatType ct = map (\(name, squad) -> (name, ct, squad))

titleCombatType :: CombatType -> String
titleCombatType Melee  = "(melee)"
titleCombatType Ranged = "(ranged)"


data AnalysisOrder = ByAttacker | ByTarget

type NamedEqUnit = (String, CombatType, [EquippedModel])

data ProbPlotType = DensityPlot | DistributionPlot | RevDistributionPlot

data AnalysisFn tgt r where
    NumWounds      :: ProbPlotType -> AnalysisFn Model        (Prob Int)
    SlainModels    :: ProbPlotType -> AnalysisFn Model        (Prob QQ)
    SlainModelsInt :: ProbPlotType -> AnalysisFn Model        (Prob Int)
    ProbKill       ::                 AnalysisFn (Int, Model) QQ
    ProbKillOne    ::                 AnalysisFn Model        QQ

data AnalysisConfig tgt r = AnalysisConfig AnalysisOrder (AnalysisFn tgt r) [NamedEqUnit] [tgt]

type AnalysisResults r = [(String, [(String, r)])]

analysisConfig' :: AnalysisOrder -> AnalysisFn tgt r -> [tgt] -> [NamedEqUnit] -> AnalysisConfig tgt r
analysisConfig' order fn tgts srcs = AnalysisConfig order fn srcs tgts

analysisFnName :: AnalysisFn tgt r -> String
analysisFnName (NumWounds _)      = "# wounds"
analysisFnName (SlainModels _)    = "# slain models"
analysisFnName (SlainModelsInt _) = "# wholly slain models"
analysisFnName ProbKill           = "" -- unused (grouped later in a bar plot)
analysisFnName ProbKillOne        = "" -- unused (grouped later in a bar plot)

analysisFnTgtName :: AnalysisFn tgt r -> tgt -> String
analysisFnTgtName (NumWounds _)      = (^.model_name)
analysisFnTgtName (SlainModels _)    = (^.model_name)
analysisFnTgtName (SlainModelsInt _) = (^.model_name)
analysisFnTgtName ProbKill           = \(n,m) -> show n ++ " " ++ m^.model_name
analysisFnTgtName ProbKillOne        = (^.model_name)

applyAnalysisFn :: AnalysisFn tgt r -> CombatType -> [EquippedModel] -> tgt -> r
applyAnalysisFn (NumWounds _)      ct srcs = numWounds ct srcs
applyAnalysisFn (SlainModels _)    ct srcs = numSlainModels ct srcs
applyAnalysisFn (SlainModelsInt _) ct srcs = numSlainModelsInt ct srcs
applyAnalysisFn ProbKill           ct srcs = uncurry $ probKill ct srcs
applyAnalysisFn ProbKillOne        ct srcs = probKill ct srcs 1

analyzeAllByAttacker :: NFData r => AnalysisFn tgt r -> [NamedEqUnit] -> [tgt] -> AnalysisResults r
analyzeAllByAttacker fn squads tgts =
    [ (title, [ trace (title ++ " " ++ legend) $ force (legend, applyAnalysisFn fn ct squad tgt)
              | tgt <- tgts
              , let legend = legendTgt tgt ])
    | (squadName, ct, squad) <- squads
    , let title = titleAtt squadName ct ]
  where
    titleAtt name ct = analysisFnName fn ++ " attacking with " ++ name ++ " " ++ titleCombatType ct
    legendTgt tgt = "vs " ++ analysisFnTgtName fn tgt

analyzeAllByTarget :: NFData r => AnalysisFn tgt r -> [NamedEqUnit] -> [tgt] -> AnalysisResults r
analyzeAllByTarget fn squads tgts =
    [ (title, [ trace (title ++ " " ++ legend) $ force (legend, applyAnalysisFn fn ct squad tgt)
              | (squadName, ct, squad) <- squads
              , let legend = legendAtt squadName ct])
    | tgt <- tgts
    , let title = titleTgt tgt ]
  where
    legendAtt name ct = "by " ++ name ++ " " ++ titleCombatType ct
    titleTgt tgt = analysisFnName fn ++ " targeting " ++ analysisFnTgtName fn tgt

eventChart :: Chart.PlotValue a => String -> [Event a] -> [Event a] -> (a, Chart.PlotLines a Chart.Percent)
eventChart title relevantEvts evts =
    (lastEvt, def & Chart.plot_lines_title  .~ title
                  & Chart.plot_lines_values .~ [[(a, Chart.Percent (realToFrac $ p*100)) | Event a p <- evts]])
  where
    lastEvt =
      let (Event a _) = last relevantEvts
      in a

densityChart :: Chart.PlotValue a => String -> Prob a -> (a, Chart.PlotLines a Chart.Percent)
densityChart title prob = eventChart title (relevantEvents dens) dens
  where
    dens = events prob

    relevantEvents :: [Event a] -> [Event a]
    relevantEvents evts =
        case takeUntilAccumPercent 0.99 evts of
            []    -> evts
            evts' -> evts'

    takeUntilAccumPercent :: Double -> [Event a] -> [Event a]
    takeUntilAccumPercent !q []                   = []
    takeUntilAccumPercent !q (e@(Event _ p) : es)
      | q <= 0    = []
      | otherwise = e : takeUntilAccumPercent (q - p) es

distributionChart :: (Ord a, Chart.PlotValue a) => String -> Prob a -> (a, Chart.PlotLines a Chart.Percent)
distributionChart title prob = eventChart title (relevantEvents dist) dist
  where
    dist = distribution prob
    relevantEvents = takeWhile (\(Event _ p) -> p <= 0.99)

revDistributionChart :: (Ord a, Chart.PlotValue a) => String -> Prob a -> (a, Chart.PlotLines a Chart.Percent)
revDistributionChart title prob = eventChart title (relevantEvents revDist) revDist
  where
    revDist = revDistribution prob
    relevantEvents = takeWhile (\(Event _ p) -> p >= 1 - 0.99)

probAnalysisChart :: (Ord a, Chart.PlotValue a) => ProbPlotType -> AnalysisResults (Prob a) -> [Chart.Layout a Chart.Percent]
probAnalysisChart plotType =
      map (uncurry buildLayout)
    . uniformLimits
    . map (_2.mapped %~ uncurry (chartFn plotType))
  where
    mb << ma = ma >> mb

    chartFn DensityPlot         = densityChart
    chartFn DistributionPlot    = distributionChart
    chartFn RevDistributionPlot = revDistributionChart

    buildLayout :: Chart.PlotValue a => String -> [Chart.PlotLines a Chart.Percent] -> Chart.Layout a Chart.Percent
    buildLayout title linePlots = def
        & Chart.layout_title .~ title
        & Chart.layout_plots .~ map Chart.toPlot (zipWith setColor moreColors linePlots)
      where
       setColor = set (Chart.plot_lines_style . Chart.line_color)

    nonEmptyTakeWhile :: (a -> Bool) -> [a] -> [a]
    nonEmptyTakeWhile pred xs =
        case takeWhile pred xs of
          []  -> xs
          xs' -> xs'

    uniformLimits :: Ord a
                  => [(String, [(a, Chart.PlotLines a Chart.Percent)])]
                  -> [(String, [Chart.PlotLines a Chart.Percent])]
    uniformLimits ls =
        case ls^..traverse._2.traverse._1 of
            [] -> []
            as -> let a = maximum as
                  in  ls & mapped._2.mapped %~ \(_,pl) ->
                        pl & Chart.plot_lines_values . mapped %~ nonEmptyTakeWhile (\(a',_) -> a' <= a)

newtype BarPlotIndex = BarPlotIndex { getBarPlotIndex :: Chart.PlotIndex }
    deriving (Eq, Ord)

instance Given [String] => Chart.PlotValue BarPlotIndex where
    toValue   = coerce (Chart.toValue @Chart.PlotIndex)
    fromValue = coerce (Chart.fromValue @Chart.PlotIndex)
    autoAxis  = coerce (Chart.autoIndexAxis @Chart.PlotIndex given)

percentAnalysisChart :: String -> AnalysisResults QQ -> (Dict (Chart.PlotValue BarPlotIndex), Chart.Layout BarPlotIndex QQ)
percentAnalysisChart title results =
    let (titles,    groups) = unzip results
        titleIndexes        = map (BarPlotIndex . fst) (Chart.addIndexes titles)

        (barTitles, values) = unzip $ map unzip groups
        sampleGroupTitles:_ = barTitles
        barValues           = zip titleIndexes (values & mapped.mapped %~ (*100))

        barPlot             = def & Chart.plot_bars_values      .~ barValues
                                  & Chart.plot_bars_titles      .~ sampleGroupTitles
                                  & Chart.plot_bars_spacing     .~ Chart.BarsFixWidth 10
                                  & Chart.plot_bars_item_styles .~ map (\(_,c) -> (Chart.FillStyleSolid c, Nothing))
                                                                       (zip sampleGroupTitles moreColors)
        layoutPlots         = case barTitles of
                                [] -> []
                                _:otherTitles
                                  | any (/= sampleGroupTitles) otherTitles -> error ("mismatching group titles: " ++ show barTitles)
                                  | otherwise                              -> [Chart.plotBars barPlot]

    in give titles (Dict, def & Chart.layout_title .~ title
                              & Chart.layout_plots .~ layoutPlots)


defaultPlotWidth, defaultPlotHeight :: Num a => a
defaultPlotWidth = 1280
defaultPlotHeight = 720

renderLayouts :: ( Chart.PlotValue x, Chart.PlotValue y
                 , Backend b V2 (N b)
                 , Renderable (Path V2 (N b)) b
                 , TypeableFloat (N b)
                 , V2 ~ V b
                 , Read (N b))
              => [Chart.Layout x y] -> IO [Diagram b]
renderLayouts frames = do
    let size = (defaultPlotWidth, defaultPlotHeight)
    env <- ChartD.defaultEnv Chart.vectorAlignmentFns defaultPlotWidth defaultPlotHeight
    return [ diag | frame <- frames
                  , let (diag, _) = ChartD.runBackend env (Chart.render (Chart.toRenderable frame) size) ]

combinePlotsVert :: (Floating (N b), Ord (N b), V b ~ V2) => [Diagram b] -> Diagram b
combinePlotsVert = vcat . map alignL

analysisFnPlot :: AnalysisFn tgt r -> AnalysisResults r -> IO (Diagram SVG)
analysisFnPlot fn results =
    fmap combinePlotsVert $
      case fn of
        NumWounds      ptype -> renderLayouts $ probAnalysisChart ptype results
        SlainModels    ptype -> renderLayouts $ probAnalysisChart ptype results
        SlainModelsInt ptype -> renderLayouts $ probAnalysisChart ptype results
        ProbKill             ->
            case percentAnalysisChart "kill probability (%)" results of
              (Dict, layout) -> renderLayouts [layout]
        ProbKillOne          ->
            case percentAnalysisChart "kill probability (%)" results of
              (Dict, layout) -> renderLayouts [layout]


analyze :: forall tgt r. NFData r => AnalysisConfig tgt r -> IO (AnalysisResults r, Diagram SVG)
analyze (AnalysisConfig order fn srcs tgts) =
    case order of
      ByAttacker -> plotResults (analyzeAllByAttacker fn srcs tgts)
      ByTarget   -> plotResults (analyzeAllByTarget   fn srcs tgts)
  where
    plotResults :: AnalysisResults r -> IO (AnalysisResults r, Diagram SVG)
    plotResults rs = do
        plot <- analysisFnPlot fn rs
        return (rs, plot)

analyzeAll :: NFData r => [AnalysisConfig tgt r] -> IO (AnalysisResults r, Diagram SVG)
analyzeAll = fmap ((concat *** combinePlotsVert) . unzip) . mapM analyze

diagramToFile :: FilePath -> Diagram SVG -> IO ()
diagramToFile path = renderSVG path absolute

resultsToSvgFile :: FilePath -> AnalysisFn tgt r -> AnalysisResults r -> IO ()
resultsToSvgFile path fn = diagramToFile path <=< analysisFnPlot fn

analysisToSvgFile :: NFData r => FilePath -> [AnalysisConfig tgt r] -> IO (AnalysisResults r)
analysisToSvgFile path cfgs = do
    (rs, diag) <- analyzeAll cfgs
    diagramToFile path diag
    return rs

mainWithPlot :: AnalysisFn tgt r -> AnalysisResults r -> IO ()
mainWithPlot fn = mainWith <=< analysisFnPlot fn

mainWithAnalysis :: NFData r => [AnalysisConfig tgt r] -> IO ()
mainWithAnalysis = mainWith . snd <=< analyzeAll
