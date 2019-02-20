{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
{-# language QuasiQuotes #-}
{-# language TypeApplications #-}
module W40K.Core.Chart.R
  ( analysisToFile
  , withEmbeddedR
  ) where

import GHC.Exts (IsList(..))
import Control.Monad (forM)

import Data.Int (Int32)
import Data.Functor (void)
import Data.Singletons (SingI(..))

import Language.R (R, SEXP, SomeSEXP(..))
import Language.R.QQ (r)

import qualified Foreign.R       as FR
import qualified Language.R      as R
import qualified Language.R.HExp as HExp
import qualified H.Prelude as H

import Unsafe.TrueName

import W40K.Core.Chart
import W40K.Core.Prob (Event(..), Prob, QQ, fmapProbMonotone,
                       events, distribution, revDistribution,
                       mean, stDev)



-- R helpers

preamble :: R s ()
preamble = void [r|
    require(tidyverse)
    require(gridExtra)
    |]

toInt32 :: Int -> Int32
toInt32 = fromIntegral

toRList :: [SomeSEXP s] -> R s (SomeSEXP s)
toRList xs = fmap R.SomeSEXP $ HExp.unhexp $ HExp.Vector (toInt32 $ length xs) (fromList xs)


-- main logic

data GGPlot s = GGPlot { plotObj :: SomeSEXP s, plotWidth :: Int, plotHeight :: Int }


tidySummaryTable :: AnalysisResultsTable (Prob QQ) -> R s (SomeSEXP s)
tidySummaryTable results = do
    let enumerate = zip [(0 :: Int32)..]

    results_r <- forM (enumerate results) $ \(i, (plotTitle, plotData)) ->
        forM (enumerate plotData) $ \(j, (legendEntry, prob)) ->
            let (m, s) = (mean prob, stDev prob)
            in [r|
                data_frame(
                    title_idx  = i_hs,
                    title      = plotTitle_hs,
                    legend_idx = j_hs,
                    legend     = legendEntry_hs,
                    mean       = m_hs,
                    std        = s_hs)
            |]

    resultsFrames <- toRList (concat results_r)
    [r|
      bind_rows(resultsFrames_hs) %>%
        mutate(
          title = str_wrap(title, width=40),
          legend = str_wrap(legend, width=40))
       |]


tidyProbResultsTable :: (Ord a, R.Literal [a] ty) => ProbPlotType -> AnalysisResultsTable (Prob a) -> R s (SomeSEXP s)
tidyProbResultsTable ptype results = do
    let enumerate = zip [(0 :: Int32)..]

    results_r <- forM (enumerate results) $ \(i, (plotTitle, plotData)) ->
        forM (enumerate plotData) $ \(j, (legendEntry, prob)) ->
            let (as, ps) = probPoints prob
            in [r|
                data_frame(
                    title_idx  = i_hs,
                    title      = plotTitle_hs,
                    legend_idx = j_hs,
                    legend     = legendEntry_hs,
                    prob       = ps_hs,
                    event      = as_hs)
            |]

    resultsFrames <- toRList (concat results_r)
    [r|
      bind_rows(resultsFrames_hs) %>%
        mutate(
          prob = 100*prob,
          title = str_wrap(title, width=40),
          legend = str_wrap(legend, width=40))
      |]
  where
    probPoints :: Ord a => Prob a -> ([a], [QQ])
    probPoints =
        case ptype of
          DensityPlot         -> unzipEvents . safeTakeUntilAccumPercent 0.99 . events
          DistributionPlot    -> unzipEvents . takeWhile (\(Event _ p) -> p <=   0.99) . distribution
          RevDistributionPlot -> unzipEvents . takeWhile (\(Event _ p) -> p >= 1-0.99) . revDistribution

    unzipEvents :: [Event a] -> ([a], [QQ])
    unzipEvents es = unzip [(a, p) | Event a p <- es]

    safeTakeUntilAccumPercent :: QQ -> [Event a] -> [Event a]
    safeTakeUntilAccumPercent !q es =
        case takeUntilAccumPercent q es of
          []  -> es
          es' -> es'

    takeUntilAccumPercent :: QQ -> [Event a] -> [Event a]
    takeUntilAccumPercent !q []                   = []
    takeUntilAccumPercent !q (e@(Event _ p) : es)
      | q <= 0    = []
      | otherwise = e : takeUntilAccumPercent (q - p) es


analysisErrBarPlot :: String -> AnalysisResultsTable (Prob QQ) -> R s (GGPlot s)
analysisErrBarPlot title results = do
    df <- tidySummaryTable results

    obj <- [r|
      facet_names = df_hs$title
      names(facet_names) = df_hs$title_idx

      df_hs %>%
        ggplot(aes(x=legend_idx, y=mean, ymin=mean-std, ymax=mean+std, fill=legend)) +
          geom_col(position=position_dodge()) +
          geom_errorbar(position=position_dodge(), width=0.5) +
          labs(
            y = paste('avg', title_hs, '± std')) +
          theme(
            text = element_text(size=6),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = 'bottom') +
          guides(
            fill = guide_legend(nrow=3)) +
          facet_wrap(
            facets = vars(title_idx),
            nrow=1,
            labeller = labeller(title_idx=facet_names))
      |]
    return (GGPlot obj 12 7)

probAnalysisChart :: (Ord a, R.Literal [a] ty) => ProbPlotType -> AnalysisResultsTable (Prob a) -> R s (GGPlot s)
probAnalysisChart ptype results = do
    df <- tidyProbResultsTable ptype results

    obj <- [r|
      facet_names = df_hs$title
      names(facet_names) = df_hs$title_idx

      df_hs %>%
        ggplot(aes(x=event, y=prob, group=legend_idx, color=legend)) +
          geom_line() +
          geom_point(size=0.6) +
          labs(
            y = 'Probability (%)') +
          theme(
            text = element_text(size=6),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = 'bottom') +
          guides(
            color = guide_legend(nrow=3)) +
          facet_wrap(
            facets = vars(title_idx),
            ncol=1,
            scales='free',
            labeller = labeller(title_idx=facet_names)) +
          scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))
      |]
    return (GGPlot obj 12 (3 + 6*length results))


plotResults :: AnalysisResults -> R s (GGPlot s)
plotResults (AnalysisResults fn results) =
    case fn of
      NumWounds      ptype -> probAnalysisChart ptype (int32results results)
      NumWoundsMax   ptype -> probAnalysisChart ptype (int32results results)
      SlainModelsInt ptype -> probAnalysisChart ptype (int32results results)
      SlainModels    ptype -> probAnalysisChart ptype results
      ProbKill             -> undefined -- analysisBarPlot "kill probability (%)" results
      ProbKillOne          -> undefined -- analysisBarPlot "kill probability (%)" results
      AverageWounds        -> undefined -- analysisBarPlot "average wounds"       results
      WoundingSummary      -> analysisErrBarPlot "wounds" results
  where
    int32results = mapResultsTable (fmapProbMonotone toInt32)


analyze :: AnalysisConfig -> R s (AnalysisResults, GGPlot s)
analyze (AnalysisConfig order fn turns tgts) =
    case order of
      ByAttacker -> plotResults' (analyzeByAttacker fn turns tgts)
      ByTarget   -> plotResults' (analyzeByTarget   fn turns tgts)
  where
    plotResults' :: AnalysisResults -> R s (AnalysisResults, GGPlot s)
    plotResults' rs = do
        plot <- plotResults rs
        return (rs, plot)


combinePlotsVert :: [GGPlot s] -> R s (GGPlot s)
combinePlotsVert plots = do
    let widths = map plotWidth plots
    let width = if null widths then 0 else maximum widths

    let heights = map plotHeight plots
        heights32 = map toInt32 heights
    let height = sum heights

    objs <- toRList (map plotObj plots)

    obj <- [r| arrangeGrob(grobs=objs_hs, ncol=1, heights=heights32_hs) |]

    return GGPlot { plotObj=obj, plotWidth=width, plotHeight=height }


savePlot :: FilePath -> GGPlot s -> R s ()
savePlot path GGPlot { plotObj=obj, plotWidth=w, plotHeight=h } =
    let w32 = toInt32 w
        h32 = toInt32 h
    in
        void [r| ggsave(filename=path_hs, plot=obj_hs, width=w32_hs, height=h32_hs, unit='cm') |]


analyzeAll :: [AnalysisConfig] -> R s ([AnalysisResults], GGPlot s)
analyzeAll cfgs = do
    (rs, plots) <- fmap unzip (mapM analyze cfgs)
    plot <- combinePlotsVert plots
    return (rs, plot)


analysisToFile :: FilePath -> [AnalysisConfig] -> IO [AnalysisResults]
analysisToFile path cfgs =
    R.runRegion $ do
      preamble
      (rs, plt) <- analyzeAll cfgs
      savePlot path plt
      return rs

withEmbeddedR :: IO a -> IO a
withEmbeddedR = R.withEmbeddedR R.defaultConfig
