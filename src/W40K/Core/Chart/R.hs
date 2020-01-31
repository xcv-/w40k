{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
{-# language QuasiQuotes #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}
module W40K.Core.Chart.R
  ( analysisToFile
  , withEmbeddedR
  ) where

import GHC.Exts (IsList(..))
import Control.Monad (forM)
import Control.Lens (over, _head)

import Data.Char (toUpper)
import Data.Int (Int32)
import Data.Functor (void)
import Data.Singletons (SingI(..))

import Language.R (R, SEXP, SomeSEXP(..))
import Language.R.QQ (r)

import qualified Foreign.R       as FR
import qualified Language.R      as R
import qualified Language.R.HExp as HExp
import qualified H.Prelude as H

import W40K.Core.Chart
import W40K.Core.Prob (Event(..), Prob, QQ, fmapProbMonotone,
                       events, distribution, revDistribution,
                       mean, stDev)
import W40K.Core.Util (capitalize)


-- R helpers

preamble :: R s ()
preamble = void [r|
    require(tidyverse)
    require(gridExtra)
    |]

toInt32 :: Int -> Int32
toInt32 = fromIntegral

toDouble :: Int -> Double
toDouble = fromIntegral

toRList :: [SomeSEXP s] -> R s (SomeSEXP s)
toRList xs = fmap R.SomeSEXP $ HExp.unhexp $ HExp.Vector (toInt32 $ length xs) (fromList xs)


-- main logic

tidyResultsTable :: AnalysisResultsTable r
                 -> (Int32 -> String -> Int32 -> String -> r -> R s (SomeSEXP s))
                 -> R s (SomeSEXP s)
tidyResultsTable results toDataFrame = do
    dfss <-
      forM (enumerate results) $ \(i, (plotTitle, plotData)) ->
        forM (enumerate plotData) $ \(j, (legendEntry, r)) ->
          toDataFrame i (capitalize plotTitle) j legendEntry r

    dfs_r <- toRList (concat dfss)
    [r| bind_rows(dfs_r_hs) |]
  where
    enumerate = zip [toInt32 0 ..]


tidyNumericTable :: R.Literal a ty => AnalysisResultsTable a -> R s (SomeSEXP s)
tidyNumericTable results = do
    df <- tidyResultsTable results $ \i title j legend a -> [r|
            tibble(
              title_idx  = i_hs,
              title      = title_hs,
              legend_idx = j_hs,
              legend     = legend_hs,
              value      = a_hs)
          |]

    [r|
      df_hs %>%
        mutate(
          title = str_wrap(title, width=40),
          legend = str_wrap(legend, width=40))
      |]


tidySummaryTable :: AnalysisResultsTable (Prob QQ) -> R s (SomeSEXP s)
tidySummaryTable results = do
    df <- tidyResultsTable results $ \i title j legend prob ->
            let (m, s) = (mean prob, stDev prob)
            in [r|
                tibble(
                  title_idx  = i_hs,
                  title      = title_hs,
                  legend_idx = j_hs,
                  legend     = legend_hs,
                  mean       = m_hs,
                  std        = s_hs)
            |]
    [r|
      df_hs %>%
        mutate(
          title = str_wrap(title, width=40),
          legend = str_wrap(legend, width=40))
      |]


tidyProbResultsTable :: (Ord a, R.Literal [a] ty) => ProbPlotType -> AnalysisResultsTable (Prob a) -> R s (SomeSEXP s)
tidyProbResultsTable ptype results = do
    df <- tidyResultsTable results $ \i title j legend prob ->
            let (as, ps) = probPoints prob
            in [r|
                tibble(
                  title_idx  = i_hs,
                  title      = title_hs,
                  legend_idx = j_hs,
                  legend     = legend_hs,
                  prob       = ps_hs,
                  event      = as_hs)
              |]
    [r|
      df_hs %>%
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



data GGPlot s = GGPlot { plotObj :: SomeSEXP s, plotWidth :: Int, plotHeight :: Int }


analysisBarPlot :: String -> AnalysisResultsTable QQ -> R s (GGPlot s)
analysisBarPlot (capitalize -> ylabel) results = do
    df <- tidyNumericTable results

    obj <- [r|
      facet_names = df_hs$title
      names(facet_names) = df_hs$title_idx

      df_hs %>%
        ggplot(aes(x=legend, y=value, fill=legend)) +
          geom_col(position=position_dodge()) +
          labs(
            y = ylabel_hs) +
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
            nrow = 1,
            labeller = labeller(title_idx=facet_names))
      |]
    return (GGPlot obj 12 7)


analysisProbBarPlot :: String -> AnalysisResultsTable QQ -> R s (GGPlot s)
analysisProbBarPlot ylabel tbl = do
    GGPlot p w h <- analysisBarPlot ylabel (mapResultsTable (*100) tbl)

    p' <- [r| p_hs + expand_limits(y = c(0, 100)) |]
    return (GGPlot p' w h)


analysisSummaryErrBarPlot :: String -> AnalysisResultsTable (Prob QQ) -> R s (GGPlot s)
analysisSummaryErrBarPlot (capitalize -> ylabel) results = do
    df <- tidySummaryTable results

    obj <- [r|
      facet_names = df_hs$title
      names(facet_names) = df_hs$title_idx

      df_hs %>%
        ggplot(aes(x=legend, y=mean, ymin=mean-std, ymax=mean+std, fill=legend)) +
          geom_col(position=position_dodge(), width=0.8) +
          geom_errorbar(position=position_dodge(), width=0.5, size=0.2) +
          labs(
            y = ylabel_hs) +
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
            nrow = 1,
            labeller = labeller(title_idx=facet_names))
      |]
    return (GGPlot obj 12 7)


probAnalysisChart :: (Ord a, R.Literal [a] ty) => String -> ProbPlotType -> AnalysisResultsTable (Prob a) -> R s (GGPlot s)
probAnalysisChart (capitalize -> xlabel) ptype results = do
    df <- tidyProbResultsTable ptype results

    obj <- [r|
      facet_names = df_hs$title
      names(facet_names) = df_hs$title_idx

      df_hs %>%
        ggplot(aes(x=event, y=prob, group=legend_idx, color=legend)) +
          geom_line(size=0.4) +
          geom_point(size=0.3) +
          labs(
            x = xlabel_hs,
            y = 'Probability (%)') +
          expand_limits(y = c(0, 100)) +
          theme(
            text = element_text(size=6),
            legend.title = element_blank(),
            legend.position = 'bottom') +
          guides(
            color = guide_legend(nrow=3)) +
          facet_wrap(
            facets = vars(title_idx),
            ncol = 1,
            scales='free',
            labeller = labeller(title_idx=facet_names)) +
          scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))
      |]
    return (GGPlot obj 12 (3 + 6*length results))


plotResults :: AnalysisResults -> R s (GGPlot s)
plotResults (AnalysisResults fn results) =
    case fn of
      NumWounds      ptype -> probAnalysisChart (xlabel ptype "wounds")              ptype (int32results results)
      NumWoundsMax   ptype -> probAnalysisChart (xlabel ptype "wounds")              ptype (int32results results)
      WoundingSummary      -> analysisSummaryErrBarPlot "Average wounds ± std"             (floatResults results)

      SlainModelsInt ptype -> probAnalysisChart (xlabel ptype "wholly slain models") ptype (int32results results)
      SlainModels    ptype -> probAnalysisChart (xlabel ptype "slain models")        ptype results
      SlainSummary         -> analysisSummaryErrBarPlot "Average killed ± std"             results

      ProbKill             -> analysisProbBarPlot "Kill probability (%)" results
      ProbKillOne          -> analysisProbBarPlot "Kill probability (%)" results
  where
    floatResults = mapResultsTable (fmapProbMonotone toDouble)
    int32results = mapResultsTable (fmapProbMonotone toInt32)

    xlabel DensityPlot         base = base
    xlabel DistributionPlot    base = "maximum " ++ base
    xlabel RevDistributionPlot base = "minimum " ++ base

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


combinePlotsVert :: String -> [GGPlot s] -> R s (GGPlot s)
combinePlotsVert name plots = do
    let widths = map plotWidth plots
    let width = if null widths then 0 else maximum widths

    let heights = map plotHeight plots
        heights32 = map toInt32 heights
    let height = sum heights

    objs <- toRList (map plotObj plots)

    obj <- [r| arrangeGrob(grobs=objs_hs, ncol=1, heights=heights32_hs, top=name_hs) |]

    return GGPlot { plotObj=obj, plotWidth=width, plotHeight=height }


savePlot :: FilePath -> GGPlot s -> R s ()
savePlot path GGPlot { plotObj=obj, plotWidth=w, plotHeight=h } =
    let w32 = toInt32 w
        h32 = toInt32 h
    in
        void [r| ggsave(filename=path_hs, plot=obj_hs, width=w32_hs, height=h32_hs, unit='cm') |]


analyzeGroup :: AnalysisConfigGroup -> R s ([AnalysisResults], GGPlot s)
analyzeGroup (AnalysisConfigGroup name cfgs) = do
    (rs, plots) <- fmap unzip (mapM analyze cfgs)
    plot <- combinePlotsVert name plots
    return (rs, plot)


analyzeAll :: String -> [AnalysisConfigGroup] -> R s ([AnalysisResults], GGPlot s)
analyzeAll name groups = do
    (rs, plots) <- fmap unzip (mapM analyzeGroup groups)
    plot <- combinePlotsVert name plots
    return (concat rs, plot)


analysisToFile :: String -> FilePath -> [AnalysisConfigGroup] -> IO [AnalysisResults]
analysisToFile path name groups =
    R.runRegion $ do
      preamble
      (rs, plt) <- analyzeAll name groups
      savePlot path plt
      return rs

withEmbeddedR :: IO a -> IO a
withEmbeddedR = R.withEmbeddedR R.defaultConfig
