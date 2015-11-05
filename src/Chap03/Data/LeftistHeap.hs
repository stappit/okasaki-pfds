{-# LANGUAGE FlexibleInstances #-}

module Chap03.Data.LeftistHeap ( --
                                 module Chap03.Data.LeftistHeap
                               , module Chap03.Data.TaggedBinaryTree
                               ) where

import Chap03.Data.TaggedBinaryTree
import Util

import Data.Monoid
import Data.Foldable
import Prelude hiding (foldr)

import Chap03.Data.Heap (Heap(..), arbHeap)
import Test.QuickCheck (Arbitrary(..), sized)

data WeightBias
data RightSpine
newtype Rank t = Rank Int
               deriving (Eq, Ord)

instance Show (Rank t) where
  show (Rank n) = show n

type SpineRank  = Rank RightSpine
type WeightRank = Rank WeightBias

instance Enum (Rank t) where
  toEnum = Rank
  fromEnum (Rank n) = n

instance Monoid (Rank RightSpine) where
  mempty = Rank 0
  (Rank m) `mappend` (Rank n) = Rank (min m n)

instance Monoid (Rank WeightBias) where
  mempty = Rank 0
  (Rank m) `mappend` (Rank n) = Rank (m + n)

newtype LeftistHeap r a = LH {unLH :: TaggedBinaryTree r a}

instance (Show r, Show a) => Show (LeftistHeap r a) where
  show (LH t) = show t

instance Functor (LeftistHeap r) where
  fmap f (LH t) = LH $ fmap f t

rank :: Monoid r => LeftistHeap r a -> r
rank = tag mempty . unLH 

isLeftistInv :: Ord a => LeftistHeap SpineRank a -> Bool
isLeftistInv (LH E) = True
isLeftistInv (LH (T _ _ a b)) = rspine a >= rspine b
    where
        rspine :: TaggedBinaryTree r a -> Int
        rspine E            = 0
        rspine (T _ _ _ b') = 1 + rspine b'

isWeightBiasedInv :: Ord a => LeftistHeap WeightRank a -> Bool
isWeightBiasedInv (LH E) = True
isWeightBiasedInv (LH (T _ _ a b)) = size a >= size b

makeT :: (Ord r, Monoid r, Enum r) => a -> LeftistHeap r a -> LeftistHeap r a -> LeftistHeap r a
makeT x a b
  | rank a >= rank b = LH $ T (succ $ rank a <> rank b) x a' b'
  | otherwise        = LH $ T (succ $ rank a <> rank b) x b' a'
  where
    LH a' = a
    LH b' = b

instance (Ord r, Monoid r, Enum r) => Heap (LeftistHeap r) where
  empty = LH E

  isEmpty (LH E) = True
  isEmpty _      = False

  merge h (LH E) = h
  merge (LH E) h = h
  merge h1@(LH (T _ x a1 b1)) h2@(LH (T _ y a2 b2))
    | x < y     = makeT x (LH a1) $ merge (LH b1) h2
    | otherwise = makeT y (LH a2) $ merge h1 (LH b2)

  insert x = merge $ makeT x e e
    where e = empty

  findMin (LH E)           = Nothing
  findMin (LH (T _ x _ _)) = Just x

  deleteMin (LH E)           = Nothing
  deleteMin (LH (T _ _ a b)) = Just $ merge (LH a) (LH b)

instance Foldable (LeftistHeap r) where
  foldr f z = foldr f z . unLH

instance (Ord a, Arbitrary a, Ord r, Monoid r, Enum r) => Arbitrary (LeftistHeap r a) where
  arbitrary = sized arbHeap
