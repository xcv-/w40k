{-# language BangPatterns #-}
{-# language ConstraintKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language RebindableSyntax #-}
module W40K.Core.ConstrMonad where

import Prelude hiding (Functor(..), Applicative(..), Monad(..), liftA2, sequence, (=<<))
import qualified Prelude

import Control.Monad.ST (ST, runST)


class NoConstr a
instance NoConstr a

class ConstrMonad c m | m -> c where
    return :: c a => a -> m a
    (>>=)  :: (c a, c b) => m a -> (a -> m b) -> m b

    default return :: Prelude.Monad m => a -> m a
    return = Prelude.return
    {-# inline return #-}

    default (>>=) :: Prelude.Monad m => m a -> (a -> m b) -> m b
    ma >>= f = ma Prelude.>>= f
    {-# inlinable (>>=) #-}

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
{-# inline liftA2 #-}

liftA3 :: (ConstrMonad c m, c a, c b, c d, c e) => (a -> b -> d -> e) -> m a -> m b -> m d -> m e
liftA3 f ma mb md = do
    a <- ma
    b <- mb
    d <- md
    return (f a b d)
{-# inline liftA3 #-}

(=<<)  :: (ConstrMonad c m, c a, c b) => (a -> m b) -> m a -> m b
f =<< ma = ma >>= f
infixr 1 =<<
{-# inline (=<<) #-}

instance ConstrMonad NoConstr IO where
instance ConstrMonad NoConstr (ST s)

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

-- sequence :: (ConstrMonad c m, c a, c [a], c (m [a])) => [m a] -> m [a]
-- sequence mas = forM mas id
-- {-# inline sequence #-}

foldlM' :: (ConstrMonad c m, c a, c b) => (b -> a -> m b) -> b -> [a] -> m b
foldlM' f !z []     = return z
foldlM' f !z (x:xs) = f z x >>= \z' -> foldlM' f z' xs
