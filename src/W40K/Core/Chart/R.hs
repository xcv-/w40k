{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
{-# language QuasiQuotes #-}
{-# language TypeApplications #-}
module W40K.Core.Chart.R
  ( analysisToFile
  ) where

import Control.Monad (forM, (<=<))
import Control.Monad.IO (MonadIO(liftIO))
import Data.Int (Int32)
import Data.Functor (void)
import Data.Singletons (SingI(..))

import Language.R (R, SEXP, SomeSEXP(..))
import Language.R.QQ (r)

import qualified Foreign.R       as FR
import qualified Language.R      as R
import qualified Language.R.HExp as HExp

import W40K.Core.Chart
import W40K.Core.Prob (Event(..), Prob, QQ, fmapProbMonotone, events, distribution, revDistribution)



-- R helpers

preamble :: R s ()
preamble = void [r|
    require(tidyverse)

    require(ggplot2)
    require(gridExtra)
    |]

toInt32 :: Int -> Int32
toInt32 = fromIntegral

nilValue :: SEXP s R.Nil
nilValue = FR.release R.nilValue


toRList :: [SomeSEXP s] -> R s (SomeSEXP s)
toRList = (\ls -> [r| ls_hs |]) <=< go
  where
    go []     = return (R.SomeSEXP nilValue)
    go [x]    =
        fmap R.SomeSEXP $ R.unSomeSEXP x $ \sexp_x -> liftIO $ FR.cons sexp_x nilValue
        -- fmap R.SomeSEXP . HExp.unhexp $ R.unSomeSEXP x (\sexp_x -> FR.cons sexp_x nilValue)
    go (x:xs) = do
        sexp_xs <- fmap (R.cast @R.List sing) (go xs)
        fmap R.SomeSEXP $ R.unSomeSEXP x $ \sexp_x -> liftIO $ FR.cons sexp_x sexp_xs
        --fmap R.SomeSEXP . HExp.unhexp $ R.unSomeSEXP x (\sexp_x -> FR.cons sexp_x sexp_xs)



-- main logic

data GGPlot s = GGPlot { plotObj :: SomeSEXP s, plotWidth :: Int, plotHeight :: Int }


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
    [r| bind_rows(resultsFrames_hs) |]
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




analysisBarPlot :: String -> AnalysisResultsTable QQ -> R s (GGPlot s)
analysisBarPlot = undefined

probAnalysisChart :: (Ord a, R.Literal [a] ty) => ProbPlotType -> AnalysisResultsTable (Prob a) -> R s (GGPlot s)
probAnalysisChart ptype results = do
    df <- tidyProbResultsTable ptype results
    [r| write_tsv(df_hs, 'df.tsv') |]

    obj <- [r| ggplot(df_hs, aes(x=event, y=prob, color=legend_idx)) + geom_line() |]
    return (GGPlot obj 2 1)


plotResults :: AnalysisResults -> R s (GGPlot s)
plotResults (AnalysisResults fn results) =
    case fn of
      NumWounds      ptype -> probAnalysisChart ptype (int32results results)
      NumWoundsMax   ptype -> probAnalysisChart ptype (int32results results)
      SlainModelsInt ptype -> probAnalysisChart ptype (int32results results)
      SlainModels    ptype -> probAnalysisChart ptype results
      ProbKill             -> analysisBarPlot "kill probability (%)" results
      ProbKillOne          -> analysisBarPlot "kill probability (%)" results
      AverageWounds        -> analysisBarPlot "average wounds"       results
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
    let width = if null plots then 0 else maximum (map plotWidth plots)
    let height = sum (map plotHeight plots)

    plotObjs <- toRList (map plotObj plots)

    obj <- [r| arrangeGrob(plotObjs_hs, ncol=1) |]

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
