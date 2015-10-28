module Chap03.Exercise07 where

import Chap03.Data.Heap
import Control.Applicative (liftA2, pure)
import Data.Maybe

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

instance (Arbitrary a, Ord a, Heap h) => Arbitrary (ExplicitMin h a) where
  arbitrary = sized arb
    where
      arb 0 = pure empty
      arb 1 = liftA2 insert arbitrary $ pure empty
      arb n = liftA2 merge (arb p) (arb q)
        where
          p = n `div` 2
          q = n - p

