{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module W40K.Core.Score where

import Control.Lens

import W40K.Core.Prob (Event(..), Prob, QQ, events, distribution, revDistribution)
import W40K.Core.Mechanics


absoluteScore :: Prob Int -> QQ
absoluteScore = integrate . revDistribution
  where
    integrate :: [Event Int] -> QQ
    integrate []     = error "empty distribution"
    integrate (e:es) = sum (zipWith weight (e:es) es)

    weight :: Event Int -> Event Int -> QQ
    weight (Event a p) (Event a' p') = fromIntegral (a' * (a' - a)) * p'

relativeScore :: [EquippedModel] -> Prob Int -> QQ
relativeScore srcs result = absoluteScore result / fromIntegral (sumOf (traverse.em_points) srcs)
