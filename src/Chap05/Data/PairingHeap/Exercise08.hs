module Chap05.Data.PairingHeap.Exercise08 where

import Chap02.Data.BinaryTree

instance Heap BinaryTree where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  findMin E         = Nothing
  findMin (T x _ _) = Just x

  merge h E = h
  merge E h = h
  merge (T x lx _) (T y ly _)
    | x <= y    = T x (T y ly lx) E
    | otherwise = T y (T x lx ly) E

  insert x = merge (T x E E)

  deleteMin E         = Nothing
  deleteMin (T _ l _) = Just (mergePairs l)
    where
      mergePairs :: Ord a => BinTree a -> BinTree a
      mergePairs (T l1 x (T l2 y rest)) =
        let h1 = T l1 x E
            h2 = T l2 y E
        in  merge (merge h1 h2) (mergePairs rest)
      mergePairs t = t
