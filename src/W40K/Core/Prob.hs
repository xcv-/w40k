{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language RebindableSyntax #-}
{-# language TypeFamilies #-}
module W40K.Core.Prob
  ( QQ
  , Event(..)
  , Prob
  , events
  , traceLength
  , traceEvents
  , traceNumEvents
  , fmapProb
  , fmapProbMonotone
  , (|>>=|), (|=<<|)
  , uniformly
  , probTrue
  , probFalse
  , probOf
  , given
  , bernoulli
  , binomial
  , foldlProbs'
  , foldrProbs
  , foldProbs
  , sumProbs
  , foldAssocIID
  , foldIID
  , sumIID
  , addImpossibleEvents
  , cdf
  , ccdf
  , summary
  , summaryInt
  , mean
  , variance
  , stDev
  , maxEvent
  ) where

import Prelude hiding (Functor(..), Applicative(..), Monad(..))
import GHC.Exts (IsList(..))

import Control.DeepSeq (NFData(..))
import Control.Monad.ST (ST, runST)

import Data.Coerce (coerce)
import Data.List (foldl', sort, sortBy, span)
import Data.MemoTrie (memo, memo2)
import Data.Monoid (Sum(..))

import Numeric.SpecFunctions (choose)

import Debug.Trace (trace)

import W40K.Core.SortedList (SortedList, SortedListItem(..))
import qualified W40K.Core.SortedList as SortedList

import W40K.Core.ConstrMonad as ConstrMonad
import W40K.Core.Util


type QQ = Double

data Event a = Event !a {-# unpack #-} !QQ
    deriving (Eq, Ord)

mapEvent :: (a -> b) -> Event a -> Event b
mapEvent f (Event a p) = Event (f a) p
{-# inline mapEvent #-}

instance NFData a => NFData (Event a) where
    rnf (Event a _) = rnf a

instance Ord a => SortedListItem (Event a) where
    type ItemKey (Event a) = a
    itemKey (Event a _) = a
    combineItems (Event _ p) (Event a p') = Event a (p + p')

instance Show a => Show (Event a) where
    show (Event a p) = show a ++ ": " ++ show (realToFrac p :: Float)


newtype Prob a = Prob { density :: SortedList (Event a) }
    deriving (Eq, Ord, Show, NFData)

events :: Prob a -> [Event a]
events (Prob es) = SortedList.toAscList es
{-# inline events #-}

traceLength :: [a] -> [a]
traceLength as = trace (show (length as)) as

traceEvents :: Show a => Prob a -> Prob a
traceEvents prob@(Prob es) =
    trace (show (events prob)) prob

traceNumEvents :: Prob a -> Prob a
traceNumEvents prob@(Prob es) =
    trace (show (length (events prob))) prob

fmapProb :: Ord b => (a -> b) -> Prob a -> Prob b
fmapProb f (Prob evts) = Prob (SortedList.map (mapEvent f) evts)
{-# inline fmapProb #-}

{-# rules "fmap/fmapProb" fmap = fmapProb #-}
{-# specialize fmapProb :: (a -> Int) -> Prob a -> Prob Int #-}

fmapProbMonotone :: Ord b => (a -> b) -> Prob a -> Prob b
fmapProbMonotone f (Prob evts) = Prob (SortedList.mapMonotone (mapEvent f) evts)
{-# inline fmapProbMonotone #-}

bindProbWithStrat :: Ord b => (forall c. [c] -> [c]) -> Prob a -> (a -> Prob b) -> Prob b
bindProbWithStrat evalList (Prob evts) f =
    case SortedList.toAscList evts of
      [Event a _] -> f a
      evtList     -> Prob $ SortedList.concat $ evalList $ totalProb [Event (f a) p | Event a p <- evtList]
  where
    totalProb :: [Event (Prob a)] -> [SortedList (Event a)]
    totalProb ess = [SortedList.fromAscList [Event b (p*p') | Event b p' <- events y] | Event y p <- ess]

{-# inlinable bindProbWithStrat #-}


instance ConstrMonad Ord Prob where
    return a = Prob (SortedList.singleton (Event a 1))
    {-# inline return #-}
    (>>=) = bindProbWithStrat seqItems
    {-# inline (>>=) #-}
    {-# specialize (>>=) :: Ord a => Prob a -> (a -> Prob Int) -> Prob Int #-}

(|>>=|) :: (Ord a, Ord b) => Prob a -> (a -> Prob b) -> Prob b
(|>>=|) = bindProbWithStrat parItems
{-# inline (|>>=|) #-}

(|=<<|) :: (Ord a, Ord b) => (a -> Prob b) -> Prob a -> Prob b
f |=<<| ma = ma |>>=| f
{-# inline (|=<<|) #-}


uniformly :: Ord a => [a] -> Prob a
uniformly as = Prob $ SortedList.fromAscList [Event a (1/n) | a <- sort as]
  where
    n = fromIntegral (length as)

probTrue :: Prob Bool -> QQ
probTrue prob =
    case events prob of
        [Event False _, Event True p] -> p
        [Event True p]                -> p
        [Event False q]               -> 1 - q
        []                            -> error "empty Prob!"
        _                             -> error ("unnormalized Prob!" ++ show prob)

probFalse :: Prob Bool -> QQ
probFalse prob = 1 - probTrue prob

probOf :: (a -> Bool) -> Prob a -> QQ
probOf test prob = sum [p | Event a p <- events prob, test a]

given :: (a -> Bool) -> Prob a -> Prob a
given hyp prob =
    Prob (SortedList.fromAscList [Event a (p / probHyp) | Event a p <- events prob, hyp a])
  where
    probHyp = probOf hyp prob

bernoulli :: QQ -> Prob Bool
bernoulli p = Prob $ SortedList.fromAscList [Event False (1-p), Event True p]

binomial :: Int -> QQ -> Prob Int
binomial 0 _ = return 0
binomial n p = binomialFlip p n

binomialFlip :: QQ -> Int -> Prob Int
binomialFlip p
  | p == 1/2  = binomial12
  | p == 1/3  = binomial13
  | p == 1/6  = binomial16
  | p == 2/3  = binomial23
  | otherwise = binomialMemo p

binomialMemo :: QQ -> Int -> Prob Int
binomialMemo p = memo $ \n ->
    case p of
      0 -> return 0
      1 -> return n
      _ -> Prob $ SortedList.fromAscList [Event k (binomProbOf n p k) | k <- [0..n]]
  where
    binomProbOf :: Int -> QQ -> Int -> QQ
    binomProbOf n p k
      | k < 0 || k > n = 0
      | n == 0         = 1
      | otherwise      = realToFrac (choose n k) * p^k * (1-p)^fromIntegral (n-k)

binomial23 :: Int -> Prob Int
binomial23 = binomialMemo (2/3)

binomial12 :: Int -> Prob Int
binomial12 = binomialMemo (1/2)

binomial13 :: Int -> Prob Int
binomial13 = binomialMemo (1/3)

binomial16 :: Int -> Prob Int
binomial16 = binomialMemo (1/6)

foldlProbs' :: (Ord a, Ord b) => (b -> a -> b) -> Prob b -> [Prob a] -> Prob b
foldlProbs' = foldl' . liftA2

foldrProbs :: (Ord a, Ord b) => (a -> b -> b) -> Prob b -> [Prob a] -> Prob b
foldrProbs = foldr . liftA2

foldProbs :: (Ord m, Monoid m) => [Prob m] -> Prob m
foldProbs = foldlProbs' mappend (return mempty)

sumProbs :: (Ord a, Num a) => [Prob a] -> Prob a
sumProbs []     = return 0
sumProbs [p]    = p
sumProbs (p:ps) = foldlProbs' (+) p ps
{-# specialize sumProbs :: [Prob Int] -> Prob Int #-}

foldAssocIID :: (Ord a) => (a -> a -> a) -> a -> Int -> Prob a -> Prob a
foldAssocIID f z 0 _ = return z
foldAssocIID f z n p
  | n < 0     = error "foldAssocIID: n must be >= 0"
  | otherwise = noCheck f z n p
  where
    noCheck f _ 1 p = p
    noCheck f z n p
      | n `mod` 2 == 0 = twiceHalf
      | otherwise      = liftA2 f p twiceHalf
      where
        !twiceHalf = liftA2 f recHalf recHalf
        !recHalf   = foldAssocIID f z (n`div`2) p

foldIID :: (Ord a, Monoid a) => Int -> Prob a -> Prob a
foldIID = foldAssocIID mappend mempty

-- TODO: Option to use Central Limit Theorem for sufficiently large n
sumIID :: (Ord a, Num a) => Int -> Prob a -> Prob a
sumIID = foldAssocIID (+) 0

addImpossibleEvents :: (Ord a, Enum a) => Prob a -> Prob a
addImpossibleEvents prob =
    let es = events prob
        as = [a | Event a _ <- es]
    in
        Prob $ SortedList.fromAscList $ merge es (zipWith Event [minimum as .. maximum as] (repeat 0))
  where
    merge es []  = es
    merge [] es' = es'
    merge ees@(e@(Event a _):es)
          ees'@(e'@(Event a' _):es')
      | a == a' = e : merge es es'
      | a <  a' = e : merge es ees'
      | a >  a' = e' : merge ees es'

cdf :: Ord a => Prob a -> [Event a]
cdf = scanl1 sumEvents . events
  where
    sumEvents (Event a p) (Event a' p') = Event a' (p + p')

ccdf :: Ord a => Prob a -> [Event a]
ccdf = scanr1 sumEvents . events
  where
    sumEvents (Event a p) (Event a' p') = Event a (p + p')

icdf :: Ord a => Prob a -> [Event a]
icdf = undefined

summary :: Prob QQ -> IO ()
summary p =
  let mu    = "µ=" ++ show (realToFrac (mean p) :: Double)
      sigma = "σ=" ++ show (sqrt $ realToFrac (variance p) :: Double)
  in
      putStrLn $ mu ++ ", " ++ sigma

summaryInt :: Prob Int -> IO ()
summaryInt = summary . fmap fromIntegral

mean :: Prob QQ -> QQ
mean prob = sum [k * p | Event k p <- events prob]

variance :: Prob QQ -> QQ
variance prob = sum [k^2 * p | Event k p <- events prob] - mean prob^2

stDev :: Prob QQ -> QQ
stDev prob = sqrt (variance prob)

maxEvent :: Ord a => Prob a -> a
maxEvent x = maximum [a | Event a _ <- events x]
