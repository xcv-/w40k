{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
module W40K.Core.Chart where

import Control.Monad (forM_, (<=<))
import Control.Monad.State (evalState, evalStateT, execStateT)
import Control.DeepSeq (NFData, force)

import Diagrams.Prelude hiding (Renderable(..), trace)
import Diagrams.Backend.CmdLine (mainWith)
import Diagrams.Backend.SVG.CmdLine (SVG)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import Debug.Trace

import W40K.Core.Prob (Event(..), Prob, events, densityToDistribution)
import W40K.Core.Mechanics

data AnalysisOrder = ByAttacker | ByTarget
type AnalysisFun a = (String, CombatType -> [EquippedModel] -> Model -> Prob a)
type NamedEqUnit = (String, CombatType, [EquippedModel])

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

analyzeAllByAttacker :: NFData a => AnalysisFun a -> [NamedEqUnit] -> [Model] -> [(String, [(String, Prob a)])]
analyzeAllByAttacker (testName, test) squads tgts =
    [ (title, [ trace (title ++ " " ++ legend) $ force (legend, test ct squad tgt)
              | tgt <- tgts
              , let legend = legendTgt tgt ])
    | (squadName, ct, squad) <- squads
    , let title = titleAtt squadName ct ]
  where
    titleAtt name ct = testName ++ " attacking with " ++ name ++ " " ++ titleCombatType ct
    legendTgt model = "vs " ++ model^.model_name

analyzeAllByTarget :: NFData a => AnalysisFun a -> [NamedEqUnit] -> [Model] -> [(String, [(String, Prob a)])]
analyzeAllByTarget (testName, test) squads tgts =
    [ (title, [ trace (title ++ " " ++ legend) $ force (legend, test ct squad tgt)
              | (squadName, ct, squad) <- squads
              , let legend = legendAtt squadName ct])
    | tgt <- tgts
    , let title = titleTgt tgt ]
  where
    legendAtt name ct = "by " ++ name ++ " " ++ titleCombatType ct
    titleTgt model = testName ++ " targeting " ++ model^.model_name

densityChart :: PlotValue a => String -> Prob a -> EC l (a, PlotLines a Percent)
densityChart title dist =
    let evts    = relevantEvents dist
        lastEvt = let (Event a _) = last evts in a
    in
        fmap ((,) lastEvt)
             (line title [[ (a, Percent (realToFrac $ p*100)) | Event a p <- events dist]])
  where
    relevantEvents :: Prob a -> [Event a]
    relevantEvents dist' =
        case takeUntilPercent 0.99 (events dist') of
            []   -> events dist'
            evts -> evts

    takeUntilPercent :: Double -> [Event a] -> [Event a]
    takeUntilPercent !q []                   = []
    takeUntilPercent !q (e@(Event _ p) : es)
      | q <= 0    = []
      | otherwise = e : takeUntilPercent (q - p) es

distributionChart :: (Ord a, PlotValue a) => String -> Prob a -> EC l (a, PlotLines a Percent)
distributionChart title dist = densityChart title (densityToDistribution dist)

plotAnalysis :: (Ord a, PlotValue a) => [(String, [(String, Prob a)])] -> [Layout a Percent]
plotAnalysis =
      map (exec . uncurry foldPlots)
    . map (_2.mapped %~ plot)
    . uniformLimits
    . map (_2.mapped %~ uncurry densityChart)
  where
    mb << ma = ma >> mb

    exec :: PlotValue a => EC (Layout a Percent) () -> Layout a Percent
    exec m = evalState (execStateT m def) (def & colors .~ moreColors)

    foldPlots :: String -> [EC (Layout a Percent) ()] -> EC (Layout a Percent) ()
    foldPlots title plots = foldr (<<) (layout_title .= title) plots

    unwrap :: Default l => EC l b -> b
    unwrap m = evalState (evalStateT m def) def

    nonEmptyTakeWhile :: (a -> Bool) -> [a] -> [a]
    nonEmptyTakeWhile pred xs =
        case takeWhile pred xs of
          []  -> xs
          xs' -> xs'

    uniformLimits :: (Ord a, Default l)
                  => [(String, [EC l (a, PlotLines a Percent)])]
                  -> [(String, [EC l (PlotLines a Percent)])]
    uniformLimits ls =
        case concatMap (map (fst . unwrap) . snd) ls of
            [] -> []
            as -> let a = maximum as
                  in  ls & mapped._2.mapped.mapped %~ \(_,pl) ->
                        pl & plot_lines_values.mapped %~ nonEmptyTakeWhile (\(a',_) -> a' <= a)

renderPlots :: PlotValue a => [Layout a Percent] -> IO [Diagram SVG]
renderPlots frames = do
    let sz@(w,h) = (1280, 720)
    env <- defaultEnv vectorAlignmentFns w h
    return [ diag | frame <- frames
                  , let (diag, _) = runBackend env (render (toRenderable frame) sz) ]

combinePlotsVert :: (Floating (N b), Ord (N b), V b ~ V2) => [Diagram b] -> Diagram b
combinePlotsVert = vcat . map alignL

mainWithPlot :: (NFData a, Ord a, PlotValue a) => [(String, [(String, Prob a)])] -> IO ()
mainWithPlot = mainWith <=< fmap combinePlotsVert . renderPlots . plotAnalysis

mainAnalysis :: (NFData a, PlotValue a)
             => [(AnalysisOrder, AnalysisFun a, [NamedEqUnit], [Model])]
             -> IO ()
mainAnalysis = mainWith <=< fmap combinePlotsVert . mconcat . map (renderPlots . plotAnalysis . analyzeAll)
  where
    analyzeAll :: NFData a
               => (AnalysisOrder, AnalysisFun a, [NamedEqUnit], [Model])
               -> [(String, [(String, Prob a)])]
    analyzeAll (ByAttacker, test, squads, tgts) = analyzeAllByAttacker test squads tgts
    analyzeAll (ByTarget,   test, squads, tgts) = analyzeAllByTarget   test squads tgts
