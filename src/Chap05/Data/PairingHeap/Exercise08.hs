module Chap05.Data.PairingHeap.Exercise08 where

import Chap05.Data.BinaryTree

instance Heap BinaryTree where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  findMin E         = Nothing
  findMin (T x _ _) = Just x

  merge h E = h
  merge E h = h
  merge (T x a1 _) (T y a2 _)
    | x <= y    = T x (T y a2 a1) E
    | otherwise = T y (T x a1 a2) E

  insert x = merge (T x E E)

  deleteMin E         = Nothing
  deleteMin (T _ a _) = Just (mergePairs a)
    where
      mergePairs :: Ord a => BinTree a -> BinTree a
      mergePairs (T a1 x (T a2 y rest)) =
        let h1 = T a1 x E
            h2 = T a2 y E
        in  merge (merge h1 h2) (mergePairs rest)
      mergePairs t = t
