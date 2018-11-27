{-# language BangPatterns #-}
{-# language ConstraintKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language RebindableSyntax #-}
{-# language TypeOperators #-}
module W40K.Core.Prob
  ( QQ
  , (:*:)(..)
  , Event(..)
  , Prob
  , events
  , traceLength
  , traceEvents
  , traceNumEvents
  , NoConstr(..)
  , ConstrMonad(..)
  , fmap
  , liftA2
  , liftA3
  , (=<<)
  , (|>>=|), (|=<<|)
  , ifThenElse
  , fail
  , (>>)
  , (<<)
  , forM
  --, sequence
  --, foldlM'
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
  , distribution
  , revDistribution
  , summary
  , summaryInt
  , mean
  , variance
  , maxEvent
  ) where

import Prelude hiding (Functor(..), Applicative(..), Monad(..), liftA2, sequence, (=<<))
import qualified Prelude

import GHC.Exts (IsList(..))

import Control.Monad.ST (ST, runST)
import Control.DeepSeq (NFData(..), force)
import Control.Parallel.Strategies (withStrategy, rpar, rseq, parList)

import Data.Coerce
import Data.List (foldl', sort, sortBy, span)
import Data.MemoTrie (memo, memo2)

import Data.Vector (Vector)
import Data.Vector.Mutable (STVector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import qualified Data.Vector.Algorithms.Merge as Vec (sort)

import Numeric.SpecFunctions (choose)

import Debug.Trace (trace)

type QQ = Double


data a :*: b = !a :*: !b deriving (Eq, Ord, Show)

infixl 1 :*:

instance (NFData a, NFData b) => NFData (a :*: b) where
    rnf (a :*: b) = rnf a `seq` rnf b


data Event a = Event !a !QQ deriving (Eq, Ord)

mapEvent :: (a -> b) -> Event a -> Event b
mapEvent f (Event a p) = Event (f a) p
{-# inline mapEvent #-}

instance NFData a => NFData (Event a) where
    rnf (Event a p) = rnf a

instance Show a => Show (Event a) where
    show (Event a p) = show a ++ ": " ++ show (realToFrac p :: Float)


-- MUST BE SORTED
newtype ProbV a = ProbV { densityV :: Vector (Event a) } deriving (Eq, Ord, Show, NFData)

newtype ProbL a = ProbL { density :: [Event a] } deriving (Eq, Ord, Show, NFData)
type Prob = ProbL
pattern Prob es = ProbL es

events :: Prob a -> [Event a]
events (Prob es) = toList es

traceLength :: [a] -> [a]
traceLength as = trace (show (length as)) as

traceEvents :: Show a => Prob a -> Prob a
traceEvents prob@(Prob es) =
    trace (show (events prob)) prob

traceNumEvents :: Prob a -> Prob a
traceNumEvents prob@(Prob es) =
    trace (show (length (events prob))) prob

class NoConstr a
instance NoConstr a

class ConstrMonad c m | m -> c where
    return :: c a => a -> m a
    (>>=)  :: (c a, c b) => m a -> (a -> m b) -> m b

    default return :: Prelude.Monad m => a -> m a
    return = Prelude.return
    default (>>=) :: Prelude.Monad m => m a -> (a -> m b) -> m b
    ma >>= f = ma Prelude.>>= f

infixl 1 >>=

fmap :: (ConstrMonad c m, c a, c b) => (a -> b) -> m a -> m b
fmap f ma = do
    a <- ma
    return (f a)
{-# noinline fmap #-}

liftA2 :: (ConstrMonad c m, c a, c b, c d) => (a -> b -> d) -> m a -> m b -> m d
liftA2 f ma mb = do
    a <- ma
    b <- mb
    return (f a b)
{-# inlinable liftA2 #-}

liftA3 :: (ConstrMonad c m, c a, c b, c d, c e) => (a -> b -> d -> e) -> m a -> m b -> m d -> m e
liftA3 f ma mb md = do
    a <- ma
    b <- mb
    d <- md
    return (f a b d)
{-# inlinable liftA3 #-}

(=<<)  :: (ConstrMonad c m, c a, c b) => (a -> m b) -> m a -> m b
f =<< ma = ma >>= f
infixr 1 =<<
{-# inline (=<<) #-}

instance ConstrMonad NoConstr IO where
instance ConstrMonad NoConstr (ST s)


rnfList :: [a] -> ()
rnfList []         = ()
rnfList aas@(a:as) = a `seq` rnfList as

forceList :: [a] -> [a]
forceList as = rnfList as `seq` as

fmapProbL :: Ord b => (a -> b) -> ProbL a -> ProbL b
fmapProbL f (ProbL [Event a p]) = ProbL [Event (f a) p]
fmapProbL f (ProbL evts)        =
    ProbL $ forceList $ sortBy (\(Event b _) (Event b' _) -> compare b b') $ group1 [Event (f a) p | Event a p <- evts]
  where
    group1 :: Ord a => [Event a] -> [Event a]
    group1 []     = []
    group1 (e:es) = group e es

    group :: Ord a => Event a -> [Event a] -> [Event a]
    group e@(Event a p) ees =
      case ees of
        [] -> [e]
        e'@(Event a' p') : es
          | a == a'   -> group (Event a (p + p')) es
          | otherwise -> e : group e' es

{-# rules "fmap/fmapProbL" fmap = fmapProbL #-}

bindProbWithStratL :: Ord b => (forall c. [c] -> [c]) -> ProbL a -> (a -> ProbL b) -> ProbL b
bindProbWithStratL evalList (ProbL [Event a _]) f = f a
bindProbWithStratL evalList (ProbL evts) f =
    ProbL $ forceList $ group1 $ merge1 $ evalList $ totalProb [Event (f a) p | Event a p <- evts]
  where
    totalProb :: [Event (ProbL a)] -> [[Event a]]
    totalProb ess = [[Event b (p*p') | Event b p' <- es] | Event (ProbL es) p <- ess]

    merge1 :: Ord a => [[Event a]] -> [Event a]
    merge1 []       = []
    merge1 (es:ess) = merge es ess

    merge :: Ord a => [Event a] -> [[Event a]] -> [Event a]
    merge es []             = es
    merge es (es':[])       = merge2 es es'
    merge es (es':es'':ess) = merge2 (merge2 es es') (merge es'' ess)

    merge2 :: Ord a => [Event a] -> [Event a] -> [Event a]
    merge2 as         []         = as
    merge2 []         bs         = bs
    merge2 aas@(a:as) bbs@(b:bs)
      | a <= b    = a : merge2 as bbs
      | otherwise = b : merge2 aas bs

    group1 :: Ord a => [Event a] -> [Event a]
    group1 []     = []
    group1 (e:es) = group e es

    group :: Ord a => Event a -> [Event a] -> [Event a]
    group e@(Event a p) ees =
      case ees of
        [] -> [e]
        e'@(Event a' p') : es
          | a == a'   -> group (Event a (p + p')) es
          | otherwise -> e : group e' es

{-# inlinable bindProbWithStratL #-}
{-# specialize bindProbWithStratL :: (forall c. [c] -> [c]) -> ProbL a -> (a -> ProbL Int) -> ProbL Int #-}
{-# specialize bindProbWithStratL :: (forall c. [c] -> [c]) -> ProbL a -> (a -> ProbL QQ)  -> ProbL QQ  #-}

bindProbWithStratV :: Ord b => (forall c. [c] -> [c]) -> ProbV a -> (a -> ProbV b) -> ProbV b
bindProbWithStratV evalList (ProbV evts) f =
    ProbV $ group1 $ merge1 $ evalList $ map (mapEvent (densityV . f)) $ Vec.toList evts
  where
    merge1 :: Ord a => [Event (Vector (Event a))] -> Vector (Event a)
    merge1 []       = Vec.empty
    merge1 (es:ess) =
        case merge es ess of
            Event merged _ -> merged

    merge :: Ord a => Event (Vector (Event a)) -> [Event (Vector (Event a))] -> Event (Vector (Event a))
    merge es []             = es
    merge es (es':[])       = merge2 es es'
    merge es (es':es'':ess) = merge2 (merge2 es es') (merge es'' ess)

    merge2 :: Ord a => Event (Vector (Event a)) -> Event (Vector (Event a)) -> Event (Vector (Event a))
    merge2 eas@(Event as _) ebs@(Event bs _) = runST $ do
        mv <- MVec.new $ Vec.length as + Vec.length bs
        go_merge2 0 mv 0 eas 0 ebs
        v  <- Vec.unsafeFreeze mv
        return (Event v 1)

    go_merge2 :: Ord a => Int -> STVector s (Event a) -> Int -> Event (Vector (Event a)) -> Int -> Event (Vector (Event a)) -> ST s ()
    go_merge2 !i mv !j eas@(Event as p1) !k ebs@(Event bs p2)
      | j >= Vec.length as && k >= Vec.length bs = return ()
      | j >= Vec.length as                       = Vec.copy (MVec.drop i mv) $ Vec.map (mulEvt p2) (Vec.drop k bs)
      | k >= Vec.length bs                       = Vec.copy (MVec.drop i mv) $ Vec.map (mulEvt p1) (Vec.drop j as)
      | a <= b                                   = MVec.write mv i (Event a (p1*p1')) >> go_merge2 (i+1) mv (j+1) eas k ebs
      | otherwise                                = MVec.write mv i (Event b (p2*p2')) >> go_merge2 (i+1) mv j eas (k+1) ebs
      where
        ~(Event a p1') = as Vec.! j
        ~(Event b p2') = bs Vec.! k

        mulEvt p (Event a p') = Event a (p*p')

    group1 :: Eq a => Vector (Event a) -> Vector (Event a)
    group1 = Vec.fromList . Vec.foldr combine []
      where
        combine :: Eq a => Event a -> [Event a] -> [Event a]
        combine e [] = [e]
        combine e@(Event a p) ees@(Event a' p' : es)
          | a == a'   = let !combined = Event a (p + p')
                        in  combined : es
          | otherwise = e : ees

{-# inlinable bindProbWithStratV #-}
{-# specialize bindProbWithStratV :: (forall c. [c] -> [c]) -> ProbV a -> (a -> ProbV Int) -> ProbV Int #-}
{-# specialize bindProbWithStratV :: (forall c. [c] -> [c]) -> ProbV a -> (a -> ProbV QQ)  -> ProbV QQ  #-}


instance ConstrMonad Ord ProbL where
    return a = ProbL [Event a 1]
    {-# inline return #-}
    (>>=) = bindProbWithStratL forceList
    {-# inline (>>=) #-}
    {-# specialize (>>=) :: Ord a => ProbL a -> (a -> ProbL Int) -> ProbL Int #-}
    {-# specialize (>>=) :: Ord a => ProbL a -> (a -> ProbL QQ)  -> ProbL QQ  #-}

instance ConstrMonad Ord ProbV where
    return a = ProbV (Vec.singleton (Event a 1))
    {-# inline return #-}
    (>>=) = bindProbWithStratV forceList
    {-# inline (>>=) #-}
    {-# specialize (>>=) :: Ord a => ProbV a -> (a -> ProbV Int) -> ProbV Int #-}
    {-# specialize (>>=) :: Ord a => ProbV a -> (a -> ProbV QQ)  -> ProbV QQ  #-}

(|>>=|) :: (Ord a, Ord b) => Prob a -> (a -> Prob b) -> Prob b
(|>>=|) = bindProbWithStratL (withStrategy $ parList rpar)
{-# inline (|>>=|) #-}

(|=<<|) :: (Ord a, Ord b) => (a -> Prob b) -> Prob a -> Prob b
f |=<<| ma = ma |>>=| f
{-# inline (|=<<|) #-}

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f
{-# inline ifThenElse #-}

fail :: a
fail = error "fail called"

(>>) :: (ConstrMonad c m, c a, c b) => m a -> m b -> m b
ma >> mb = ma >>= \_ -> mb
infixl 1 >>
{-# inline (>>) #-}

(<<) :: (ConstrMonad c m, c a, c b) => m b -> m a -> m b
mb << ma = ma >> mb
infixr 1 <<
{-# inline (<<) #-}

forM :: (ConstrMonad c m, c b, c [b], c (m [b])) => [a] -> (a -> m b) -> m [b]
forM []     f = return []
forM (a:as) f = do
    b  <- f a
    bs <- forM as f
    return (b:bs)
{-# inlinable forM #-}
--
-- sequence :: (ConstrMonad c m, c a, c [a], c (m [a])) => [m a] -> m [a]
-- sequence mas = forM mas id
-- {-# inline sequence #-}
--
-- foldlM' f z []     = return z
-- foldlM' f z (x:xs) = do
--     y <- f z x
--     y `seq` foldlM' f y xs

uniformly :: Ord a => [a] -> Prob a
uniformly as = Prob $ fromList [Event a (1/n) | a <- sort as]
  where
    n = fromIntegral (length as)

probTrue :: Prob Bool -> QQ
probTrue prob =
    case events prob of
        [Event False _, Event True p] -> p
        [Event True p]                -> p
        [Event False q]               -> 1 - q
        []                            -> error "empty Prob!"
        _                             -> error "unnormalized Prob!"

probFalse :: Prob Bool -> QQ
probFalse prob = 1 - probTrue prob

probOf :: (a -> Bool) -> Prob a -> QQ
probOf test prob = sum [p | Event a p <- events prob, test a]

given :: (a -> Bool) -> Prob a -> Prob a
given hyp prob =
    Prob [Event a (p / probHyp) | Event a p <- events prob, hyp a]
  where
    probHyp = probOf hyp prob

bernoulli :: QQ -> Prob Bool
bernoulli p = Prob $ fromList [Event False (1-p), Event True p]

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
      0 -> force $ return 0
      1 -> force $ return n
      _ -> force $ Prob $ fromList [Event k (binomProbOf n p k) | k <- [0..n]]
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

foldlProbs' :: (Ord a, Ord b) => (b -> a -> b) -> b -> [Prob a] -> Prob b
foldlProbs' _ z []     = return z
foldlProbs' f z (p:ps) = do
    z' <- fmap (f z) p
    z' `seq` foldlProbs' f z' ps

foldrProbs :: (Ord a, Ord b) => (a -> b -> b) -> b -> [Prob a] -> Prob b
foldrProbs f z = foldr (liftA2 f) (return z)

foldProbs :: (Ord m, Monoid m) => [Prob m] -> Prob m
foldProbs = foldlProbs' mappend mempty

sumProbs :: (Ord a, Num a) => [Prob a] -> Prob a
sumProbs = foldlProbs' (+) 0

distribution :: Ord a => Prob a -> [Event a]
distribution (Prob evts) = scanl1 sumEvents evts
  where
    sumEvents (Event a p) (Event a' p') = Event a' (p + p')

revDistribution :: Ord a => Prob a -> [Event a]
revDistribution (Prob evts) = scanr1 sumEvents evts
  where
    sumEvents (Event a p) (Event a' p') = Event a (p + p')

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

maxEvent :: Ord a => Prob a -> a
maxEvent (Prob es) = maximum [a | Event a _ <- es]
