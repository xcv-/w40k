{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
module W40K.Core.Chart
  ( UnitOf(..)
  , AnalysisOrder(..)
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

import W40K.Core.Prob (Prob, QQ {- , addImpossibleEvents -})
import W40K.Core.Mechanics
import W40K.Core.Util (whnfItems)


data AnalysisOrder = ByAttacker | ByTarget

data ProbPlotType = PlotDF | PlotCDF | PlotCCDF

data AnalysisFn tgt r where
    NumWounds       :: AsModel tgt => ProbPlotType -> AnalysisFn tgt          (Prob Int)
    NumWoundsMax    :: AsModel tgt => ProbPlotType -> AnalysisFn tgt          (Prob Int)
    WoundingSummary :: AsModel tgt =>                 AnalysisFn tgt          (Prob Int)
    SlainModels     :: AsModel tgt => ProbPlotType -> AnalysisFn tgt          (Prob QQ)
    SlainModelsInt  :: AsModel tgt => ProbPlotType -> AnalysisFn tgt          (Prob Int)
    SlainSummary    :: AsModel tgt =>                 AnalysisFn tgt          (Prob QQ)
    ProbKillOne     :: AsModel tgt =>                 AnalysisFn tgt          QQ
    ProbKill        :: AsModel tgt =>                 AnalysisFn (UnitOf tgt) QQ


data AnalysisConfig = forall tgt r. AnalysisConfig
  { analysisOrder :: AnalysisOrder
  , analysisFn    :: AnalysisFn tgt r
  , attackers     :: [GenericTurn tgt]
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
analysisFnName ProbKillOne        = "kill probability"
analysisFnName ProbKill           = "kill probability"

analysisFnTgtName :: AnalysisFn tgt r -> tgt -> String
analysisFnTgtName (NumWounds _)      = (^.as_model.model_name)
analysisFnTgtName (NumWoundsMax _)   = (^.as_model.model_name)
analysisFnTgtName WoundingSummary    = (^.as_model.model_name)
analysisFnTgtName (SlainModels _)    = (^.as_model.model_name)
analysisFnTgtName (SlainModelsInt _) = (^.as_model.model_name)
analysisFnTgtName SlainSummary       = (^.as_model.model_name)
analysisFnTgtName ProbKillOne        = (^.as_model.model_name)
analysisFnTgtName ProbKill           = \(UnitOf n m) -> show n ++ " " ++ m^.as_model.model_name

applyAnalysisFn :: (Ord pr, Ord cr) => AnalysisFn tgt r -> Turn pr cr tgt -> tgt -> r
applyAnalysisFn fn turn tgt =
    case fn of
      NumWounds _      -> {- addImpossibleEvents $ -} turnNumWounds turn tgt
      NumWoundsMax _   -> {- addImpossibleEvents $ -} turnNumWoundsMax turn tgt
      WoundingSummary  -> turnNumWounds turn tgt

      SlainModels _    -> turnNumSlainModels turn tgt
      SlainModelsInt _ -> {- addImpossibleEvents $ -} turnNumSlainModelsInt turn tgt
      SlainSummary     -> turnNumSlainModels turn tgt

      ProbKillOne      -> turnProbKill (turn & turnTarget %~ unitMember) (UnitOf 1 tgt)
      ProbKill         -> turnProbKill turn tgt


forceResults :: AnalysisResults -> AnalysisResults
forceResults (AnalysisResults fn results) =
    whnfItems [whnfItems [r | (_, r) <- rs] | (_, rs) <- results]
      `seq` AnalysisResults fn results


analyzeByAttacker :: AnalysisFn tgt r -> [GenericTurn tgt] -> [tgt] -> AnalysisResults
analyzeByAttacker fn turns tgts =
    forceResults $ AnalysisResults fn
      [ (title,
          [ trace traceText (legend, applyAnalysisFn fn turn tgt)
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

analyzeByTarget :: AnalysisFn tgt r -> [GenericTurn tgt] -> [tgt] -> AnalysisResults
analyzeByTarget fn turns tgts =
    forceResults $ AnalysisResults fn
      [ (title,
          [ trace traceText (legend, applyAnalysisFn fn turn tgt)
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

