{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
module W40K.Core.Util
  ( seqItems
  , parItems
  , (:*:)(..)
  , groupWith
  ) where

import Control.DeepSeq (NFData(..))
import Control.Parallel.Strategies (withStrategy, rpar, parList)

import Data.List (groupBy)


whnfItems :: [a] -> ()
whnfItems []         = ()
whnfItems aas@(a:as) = a `seq` whnfItems as

seqItems :: [a] -> [a]
seqItems as = whnfItems as `seq` as

parItems :: [a] -> [a]
parItems = withStrategy (parList rpar)


data a :*: b = !a :*: !b deriving (Eq, Ord, Show)

infixl 1 :*:

instance (NFData a, NFData b) => NFData (a :*: b) where
    rnf (a :*: b) = rnf a `seq` rnf b


groupWith :: forall a b. (a -> a -> Bool) -> (a -> [a] -> b) -> [a] -> [b]
groupWith eqrel fold = map combine . groupBy eqrel
  where
    combine :: [a] -> b
    combine []     = error "groupWith: groupBy should not return empty groups"
    combine (a:as) = fold a as
