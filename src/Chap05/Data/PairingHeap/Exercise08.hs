module Chap05.Data.PairingHeap.Exercise08 where

import Chap03.Data.Heap (Heap(..), arbHeap)
import Chap05.Data.BinaryTree

import Test.QuickCheck (Arbitrary(..), sized)

newtype PairingHeap a = C5E8 (BinaryTree a)
                      deriving (Show)

instance Heap PairingHeap where
  empty = C5E8 E

  isEmpty (C5E8 E) = True
  isEmpty _        = False

  findMin (C5E8 E)         = Nothing
  findMin (C5E8 (T x _ _)) = Just x

  merge h (C5E8 E) = h
  merge (C5E8 E) h = h
  merge (C5E8 (T x a1 _)) (C5E8 (T y a2 _))
    | x <= y    = C5E8 $ T x (T y a2 a1) E
    | otherwise = C5E8 $ T y (T x a1 a2) E

  insert x = merge (C5E8 $ T x E E)

  deleteMin (C5E8 E)         = Nothing
  deleteMin (C5E8 (T _ a _)) = Just $ mergePairs a
    where
      mergePairs :: Ord a => BinaryTree a -> PairingHeap a
      mergePairs (T a1 x (T a2 y rest)) =
        let h1 = C5E8 $ T a1 x E
            h2 = C5E8 $ T a2 y E
        in  merge (merge h1 h2) (mergePairs rest)
      mergePairs t = C5E8 t

instance (Ord a, Arbitrary a) => Arbitrary (PairingHeap a) where
  arbitrary = sized arbHeap
