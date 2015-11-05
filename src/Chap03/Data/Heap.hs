{-# LANGUAGE FlexibleInstances #-}

module Chap03.Data.Heap where

import Test.QuickCheck (Arbitrary(..), sized, choose, Gen)
import Control.Monad (liftM2, return)

class Heap h where
  empty     :: Ord a => h a
  isEmpty   :: Ord a => h a -> Bool
  insert    :: Ord a =>   a -> h a -> h a
  merge     :: Ord a => h a -> h a -> h a
  findMin   :: Ord a => h a -> Maybe a     -- may be empty
  deleteMin :: Ord a => h a -> Maybe (h a) -- may be empty

arbHeap :: (Heap h, Ord a, Arbitrary a) => Int -> Gen (h a)
arbHeap 0 = return empty
arbHeap 1 = liftM2 insert arbitrary (return empty)
arbHeap n = do
    k <- choose (1, n - 1)
    liftM2 merge (arbHeap k) (arbHeap $ n - k)
