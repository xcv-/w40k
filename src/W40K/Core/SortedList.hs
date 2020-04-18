{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language Strict #-}
module W40K.Core.SortedList
  ( SortedList
  , SortedListItem(..)
  , empty
  , singleton
  , fromAscList
  , toAscList
  , map
  , mapMonotone
  , concat
  ) where

import Prelude hiding (concat, map)
import qualified Prelude

import GHC.Exts (IsList(..))

import Control.DeepSeq (NFData(..))

import Data.Coerce (coerce)
import Data.Function (on)
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Semigroup (Semigroup(..))
import Data.List.NonEmpty (NonEmpty(..))


newtype SortedList a = SortedList [a]
  deriving (Eq, Ord)
  deriving newtype (NFData, Show)

seqItemsSL :: SortedList a -> SortedList a
seqItemsSL (SortedList as) = SortedList as -- (seqItems as)
{-# inline seqItemsSL #-}

empty :: SortedList a
empty = SortedList []
{-# inline empty #-}

singleton :: a -> SortedList a
singleton a = SortedList [a]
{-# inline singleton #-}

fromAscList :: [a] -> SortedList a
fromAscList = SortedList
{-# inline fromAscList #-}

toAscList :: SortedList a -> [a]
toAscList (SortedList as) = as
{-# inline toAscList #-}

instance SortedListItem a => IsList (SortedList a) where
    type Item (SortedList a) = a
    fromList = seqItemsSL . reduceSortedList . SortedList . sortBy (compare `on` itemKey)
    {-# inline fromList #-}
    toList = toAscList
    {-# inline toList #-}

class Ord (ItemKey a) => SortedListItem a where
    type ItemKey a :: Type
    itemKey :: a -> ItemKey a
    combineItems :: a -> a -> a

instance SortedListItem a => Semigroup (SortedList a) where
    as <> bs = seqItemsSL $ mergeSortedLists as bs
    {-# inline (<>) #-}
    sconcat (as :| ass) = seqItemsSL $ mergeSortedListsN (as:ass)
    {-# inline sconcat #-}

instance SortedListItem a => Monoid (SortedList a) where
    mempty = SortedList []
    {-# inline mempty #-}
    mconcat = concat
    {-# inline mconcat #-}

map :: SortedListItem b => (a -> b) -> SortedList a -> SortedList b
map f (SortedList as) = fromList (Prelude.map f as)
{-# inline map #-}

mapMonotone :: SortedListItem b => (a -> b) -> SortedList a -> SortedList b
mapMonotone f (SortedList as) = fromAscList (reduce (Prelude.map f as))
{-# inline mapMonotone #-}

concat :: SortedListItem a => [SortedList a] -> SortedList a
concat = seqItemsSL . mergeSortedListsN
{-# inline concat #-}

mergeSortedListsN :: forall a. SortedListItem a => [SortedList a] -> SortedList a
mergeSortedListsN = coerce (mergeN @a)
{-# inline mergeSortedListsN #-}

mergeSortedLists :: forall a. SortedListItem a => SortedList a -> SortedList a -> SortedList a
mergeSortedLists = coerce (merge2 @a)
{-# inline mergeSortedLists #-}

reduceSortedList :: forall a. SortedListItem a => SortedList a -> SortedList a
reduceSortedList = coerce (reduce @a)
{-# inline reduceSortedList #-}

mergeN :: forall a. SortedListItem a => [[a]] -> [a]
mergeN []       = []
mergeN (xs:xss) = go xs xss
  where
    go as []             = as
    go as (as':[])       = merge2 as as'
    go as (as':as'':ass) = merge2 (merge2 as as') (go as'' ass)
{-# inlinable mergeN #-}

merge2 :: SortedListItem a => [a] -> [a] -> [a]
merge2 as         []         = as
merge2 []         bs         = bs
merge2 aas@(a:as) bbs@(b:bs) =
  case compare (itemKey a) (itemKey b) of
    EQ -> merge2 (combineItems a b : as) bs
    LT -> a : merge2 as bbs
    GT -> b : merge2 aas bs
{-# inlinable merge2 #-}

reduce :: forall a. SortedListItem a => [a] -> [a]
reduce []     = []
reduce (x:xs) = go x xs
  where
    go a aas =
      case aas of
        [] -> [a]
        a' : as
          | itemKey a == itemKey a' -> go (combineItems a a') as
          | otherwise               -> a : go a' as
{-# inlinable reduce #-}
