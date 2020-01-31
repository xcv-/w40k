{-# language GADTs #-}
{-# language RankNTypes #-}
module W40K.Core.Chart
  ( AnalysisOrder(..)
  , ProbPlotType(..)
  , AnalysisFn(..)
  , AnalysisConfig(..)
  , AnalysisConfigGroup(..)
  , AnalysisResultsTable
  , AnalysisResults(..)
  , mapResultsTable
  , analyzeByAttacker
  , analyzeByTarget
  ) where

import Control.DeepSeq (NFData(..))
import Control.Lens
import Debug.Trace

import qualified W40K.Core.ConstrMonad as CM
import W40K.Core.Prob (Prob, QQ, fmapProbMonotone, addImpossibleEvents, mean)
import W40K.Core.Mechanics
import W40K.Core.Util (whnfItems)


data AnalysisOrder = ByAttacker | ByTarget

data ProbPlotType = DensityPlot | DistributionPlot | RevDistributionPlot

data AnalysisFn tgt r where
    NumWounds       :: IsModel tgt => ProbPlotType -> AnalysisFn tgt        (Prob Int)
    NumWoundsMax    :: IsModel tgt => ProbPlotType -> AnalysisFn tgt        (Prob Int)
    WoundingSummary :: IsModel tgt =>                 AnalysisFn tgt        (Prob Int)
    SlainModels     :: IsModel tgt => ProbPlotType -> AnalysisFn tgt        (Prob QQ)
    SlainModelsInt  :: IsModel tgt => ProbPlotType -> AnalysisFn tgt        (Prob Int)
    SlainSummary    :: IsModel tgt =>                 AnalysisFn tgt        (Prob QQ)
    ProbKill        :: IsModel tgt =>                 AnalysisFn (Int, tgt) QQ
    ProbKillOne     :: IsModel tgt =>                 AnalysisFn tgt        QQ


data AnalysisConfig = forall tgt r. AnalysisConfig
  { analysisOrder :: AnalysisOrder
  , analysisFn    :: AnalysisFn tgt r
  , attackers     :: [GenericTurn]
  , targets       :: [tgt]
  }

data AnalysisConfigGroup = AnalysisConfigGroup
  { groupName    :: String
  , groupConfigs :: [AnalysisConfig]
  }


type AnalysisResultsTable r = [(String, [(String, r)])]

data AnalysisResults = forall tgt r. AnalysisResults
  { resultsourceFn :: !(AnalysisFn tgt r)
  , resultTable    :: !(AnalysisResultsTable r)
  }

instance NFData AnalysisResults where
    rnf (AnalysisResults _ _) = ()

mapResultsTable :: (r1 -> r2) -> AnalysisResultsTable r1 -> AnalysisResultsTable r2
mapResultsTable f = mapped._2.mapped._2 %~ f

analysisFnName :: AnalysisFn tgt r -> String
analysisFnName (NumWounds _)      = "wounds"
analysisFnName (NumWoundsMax _)   = "wounds"
analysisFnName WoundingSummary    = "wounding summary"
analysisFnName (SlainModels _)    = "slain models"
analysisFnName (SlainModelsInt _) = "wholly slain models"
analysisFnName SlainSummary       = "slain summary"
analysisFnName ProbKill           = "kill probability"
analysisFnName ProbKillOne        = "kill probability"

analysisFnTgtName :: AnalysisFn tgt r -> tgt -> String
analysisFnTgtName (NumWounds _)      = (^.as_model.model_name)
analysisFnTgtName (NumWoundsMax _)   = (^.as_model.model_name)
analysisFnTgtName WoundingSummary    = (^.as_model.model_name)
analysisFnTgtName (SlainModels _)    = (^.as_model.model_name)
analysisFnTgtName (SlainModelsInt _) = (^.as_model.model_name)
analysisFnTgtName SlainSummary       = (^.as_model.model_name)
analysisFnTgtName ProbKill           = \(n,m) -> show n ++ " " ++ m^.as_model.model_name
analysisFnTgtName ProbKillOne        = (^.as_model.model_name)

applyAnalysisFn :: (Ord pr, Ord cr) => AnalysisFn tgt r -> Turn pr cr -> tgt -> r
applyAnalysisFn fn turn tgt =
    case fn of
      NumWounds _      -> addImpossibleEvents $ turnNumWounds turn (tgt^.as_model)
      NumWoundsMax _   -> addImpossibleEvents $ turnNumWoundsMax turn (tgt^.as_model) (tgt^.as_model.model_wnd)
      WoundingSummary  -> turnNumWounds turn (tgt^.as_model)

      SlainModels _    -> turnNumSlainModels turn (tgt^.as_model)
      SlainModelsInt _ -> addImpossibleEvents $ turnNumSlainModelsInt turn (tgt^.as_model)
      SlainSummary     -> turnNumSlainModels turn (tgt^.as_model)

      ProbKill         -> turnProbKill turn (fst tgt) (tgt^._2.as_model)
      ProbKillOne      -> turnProbKill turn 1 (tgt^.as_model)


forceResults :: AnalysisResults -> AnalysisResults
forceResults (AnalysisResults fn results) =
    whnfItems [whnfItems [r | (_, r) <- rs] | (_, rs) <- results]
      `seq` AnalysisResults fn results


analyzeByAttacker :: AnalysisFn tgt r -> [GenericTurn] -> [tgt] -> AnalysisResults
analyzeByAttacker fn turns tgts =
    forceResults $ AnalysisResults fn
      [ (title, [ trace traceText (legend, applyAnalysisFn fn turn tgt)
                | tgt <- tgts
                , let legend = legendTgt tgt
                , let traceText = analysisFnName fn ++ " " ++ title ++ " " ++ legend
                ])
      | GenericTurn turn <- turns
      , let title = titleAtt (_turnName turn)
      ]
  where
    legendTgt tgt = "vs " ++ analysisFnTgtName fn tgt
    titleAtt name = "attacking with " ++ name

analyzeByTarget :: AnalysisFn tgt r -> [GenericTurn] -> [tgt] -> AnalysisResults
analyzeByTarget fn turns tgts =
    forceResults $ AnalysisResults fn
      [ (title, [ trace traceText (legend, applyAnalysisFn fn turn tgt)
                | GenericTurn turn <- turns
                , let legend = legendAtt (_turnName turn)
                , let traceText = analysisFnName fn ++ " " ++ title ++ " " ++ legend
                ])
      | tgt <- tgts
      , let title = titleTgt tgt
      ]
  where
    legendAtt name = "with " ++ name

    titleTgt tgt = "targeting " ++ analysisFnTgtName fn tgt

