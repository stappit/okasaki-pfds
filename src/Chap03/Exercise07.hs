module Chap03.Exercise07 where

import Control.Applicative (liftA2)
import Data.Maybe

import Chap03.Data.Heap (Heap(..), arbHeap)
import Test.QuickCheck (Arbitrary(..), sized)

data ExplicitMin h a = Empty
                     | H a (h a)
                     deriving (Show)

instance Heap h => Heap (ExplicitMin h) where
  empty = Empty
  isEmpty Empty = True
  isEmpty _     = False
  insert x Empty   = H x (insert x empty)
  insert x (H y h) = H (min x y) (insert x h)
  merge Empty    h        = h
  merge h        Empty    = h
  merge (H x h1) (H y h2) = H (min x y) (merge h1 h2)
  findMin Empty   = Nothing
  findMin (H x _) = Just x
  deleteMin Empty   = Nothing
  deleteMin (H _ h) = Just . fromMaybe Empty $ liftA2 H minh' h'
    where 
        h' = deleteMin h
        minh' = h' >>= findMin

instance (Heap h, Ord a, Arbitrary a) => Arbitrary (ExplicitMin h a) where
  arbitrary = sized arbHeap
