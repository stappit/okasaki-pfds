module Chap05.Data.PairingHeap where

import Chap03.Data.Heap (Heap(..), arbHeap)

import Test.QuickCheck (Arbitrary(..), sized)

data PairingHeap a = E
                   | T a [PairingHeap a]
                   deriving (Show)

instance Heap PairingHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  findMin E        = Nothing
  findMin (T x _) = Just x

  merge h E = h
  merge E h = h
  merge h1@(T x hs1) h2@(T y hs2)
    | x <= y    = T x (h2:hs1)
    | otherwise = T y (h1:hs2)

  insert x = merge (T x []) 

  deleteMin E        = Nothing
  deleteMin (T _ hs) = Just (mergePairs hs)
    where
      mergePairs :: Ord a => [PairingHeap a] -> PairingHeap a
      mergePairs []  = E
      mergePairs [h] = h
      mergePairs (h1:h2:rest) = merge (merge h1 h2) (mergePairs rest)

instance (Ord a, Arbitrary a) => Arbitrary (PairingHeap a) where
  arbitrary = sized arbHeap
