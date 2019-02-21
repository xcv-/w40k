{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module W40K.Core.Chart.Chart
  ( plotResults
  , analyze
  , analyzeAll
  , diagramToFile
  , resultsToSvgFile
  , analysisToSvgFile
  , mainWithPlot
  , mainWithAnalysis
  ) where

import Data.Char (toUpper)
import Data.Coerce (coerce)
import Data.Constraint (Dict(..))
import Data.Reflection (Given(..), give)

import Control.Arrow (second)
import Control.Lens
import Control.Monad (forM_, (<=<))
import Control.Monad.State (evalState, evalStateT, execStateT)
import Control.DeepSeq (NFData, force)

import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.SVG         (renderSVG)
import Diagrams.Backend.CmdLine     (mainWith)
import Diagrams.Backend.SVG.CmdLine (SVG)

import qualified Graphics.Rendering.Chart                  as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as ChartD

import W40K.Core.Chart
import W40K.Core.Prob (Event(..), Prob, QQ, events, distribution, revDistribution)


moreColors :: [AlphaColour Double]
moreColors = cycle $ map opaque
  [black, blue, brown, chocolate, cyan, darkgreen, fuchsia, gold, gray,
   greenyellow, lightpink, olive, orange, red, yellow]


defaultPlotWidth, defaultPlotHeight :: Num a => a
defaultPlotWidth = 1280
defaultPlotHeight = 720



eventChart :: Chart.PlotValue a => String -> [Event a] -> [Event a] -> (a, Chart.PlotLines a Chart.Percent)
eventChart title relevantEvts evts =
    (lastEvt, def & Chart.plot_lines_title  .~ title
                  & Chart.plot_lines_values .~ [[(a, toPercent p) | Event a p <- evts]])
  where
    toPercent p
      | p < 0     = Chart.Percent 0
      | p > 1     = Chart.Percent 100
      | otherwise = Chart.Percent (p*100)

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

probAnalysisChart :: (Ord a, Chart.PlotValue a) => String -> ProbPlotType -> AnalysisResultsTable (Prob a) -> [Chart.Layout a Chart.Percent]
probAnalysisChart xlabel plotType =
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
        & Chart.layout_x_axis . Chart.laxis_title .~ over _head toUpper xlabel
        & Chart.layout_y_axis . Chart.laxis_title .~ "Probability (%)"
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

analysisBarPlot :: String -> AnalysisResultsTable QQ -> (Dict (Chart.PlotValue BarPlotIndex), Chart.Layout BarPlotIndex QQ)
analysisBarPlot ylabel results =
    let (titles,    groups) = unzip results
        titleIndexes        = map (BarPlotIndex . fst) (Chart.addIndexes titles)

        (barTitles, values) = unzip $ map unzip groups
        sampleGroupTitles:_ = barTitles
        barValues           = zip titleIndexes values

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

    in give titles (Dict, def & Chart.layout_y_axis . Chart.laxis_title .~ over _head toUpper ylabel
                              & Chart.layout_plots .~ layoutPlots)


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

plotResults :: AnalysisResults -> IO (Diagram SVG)
plotResults (AnalysisResults fn results) =
    fmap combinePlotsVert $
      case fn of
        NumWounds      ptype -> renderLayouts $ probAnalysisChart (xlabel ptype "wounds")              ptype results
        NumWoundsMax   ptype -> renderLayouts $ probAnalysisChart (xlabel ptype "wounds")              ptype results
        SlainModels    ptype -> renderLayouts $ probAnalysisChart (xlabel ptype "wholly slain models") ptype results
        SlainModelsInt ptype -> renderLayouts $ probAnalysisChart (xlabel ptype "slain models")        ptype results
        ProbKill             ->
            case analysisBarPlot "Kill probability (%)" results of
              (Dict, layout) -> renderLayouts [layout]
        ProbKillOne          ->
            case analysisBarPlot "Kill probability (%)" results of
              (Dict, layout) -> renderLayouts [layout]
        AverageWounds        ->
            case analysisBarPlot "Average wounds" results of
              (Dict, layout) -> renderLayouts [layout]
        WoundingSummary      -> error "WoundingSummary is not supported with the Chart-diagrams backend"
  where
    xlabel DensityPlot         base = base
    xlabel DistributionPlot    base = "maximum " ++ base
    xlabel RevDistributionPlot base = "minimum " ++ base



analyze :: AnalysisConfig -> IO (AnalysisResults, Diagram SVG)
analyze (AnalysisConfig order fn turns tgts) =
    case order of
      ByAttacker -> plotResults' (analyzeByAttacker fn turns tgts)
      ByTarget   -> plotResults' (analyzeByTarget   fn turns tgts)
  where
    plotResults' :: AnalysisResults -> IO (AnalysisResults, Diagram SVG)
    plotResults' rs = do
        plot <- plotResults rs
        return (rs, plot)

analyzeAll :: [AnalysisConfig] -> IO ([AnalysisResults], Diagram SVG)
analyzeAll = fmap (second combinePlotsVert . unzip) . mapM analyze

diagramToFile :: FilePath -> Diagram SVG -> IO ()
diagramToFile path = renderSVG path absolute

resultsToSvgFile :: FilePath -> AnalysisResults -> IO ()
resultsToSvgFile path = diagramToFile path <=< plotResults

analysisToSvgFile :: FilePath -> [AnalysisConfig] -> IO [AnalysisResults]
analysisToSvgFile path cfgs = do
    (rs, diag) <- analyzeAll cfgs
    diagramToFile path diag
    return rs

mainWithPlot :: AnalysisResults -> IO ()
mainWithPlot = mainWith <=< plotResults

mainWithAnalysis :: [AnalysisConfig] -> IO ()
mainWithAnalysis = mainWith . snd <=< analyzeAll
