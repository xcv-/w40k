{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
module W40K.Core.Util
  ( whnfItems
  , seqItems
  , parItems
  , parMap
  , (:*:)(..)
  , groupWith
  , capitalize
  , filteredOn
  ) where

import Control.DeepSeq (NFData(..))
import Control.Lens (Getting, Choice, Optic', over, _head, filtered, view)
import Control.Parallel.Strategies (withStrategy, rpar, parList)

import Data.Char (toUpper)
import Data.List (groupBy)


whnfItems :: [a] -> ()
whnfItems []     = ()
whnfItems (a:as) = a `seq` whnfItems as

seqItems :: [a] -> [a]
seqItems as = whnfItems as `seq` as

parItems :: [a] -> [a]
parItems = withStrategy (parList rpar)

parMap :: (a -> b) -> [a] -> [b]
parMap f = parItems . map f


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


capitalize :: String -> String
capitalize = over _head toUpper


filteredOn :: (Choice p, Applicative f) => Getting a s a -> (a -> Bool) -> Optic' p f s s
filteredOn getter p = filtered (p . view getter)
