module Chap05.Data.SplayHeap where

import Chap03.Data.Heap
import Data.Foldable
import Prelude hiding (foldr)

data SplayHeap a = E
                 | T (SplayHeap a) a (SplayHeap a)
                 deriving (Show, Eq)

instance Foldable SplayHeap where
  foldr f z E = z
  foldr f z (T l x r) = foldr f (f x $ foldr f z r) l

bigger :: Ord a => a -> SplayHeap a -> SplayHeap a
bigger _ E = E
bigger pivot (T a x b)
  | x <= pivot = bigger pivot b
  | otherwise  = case a of
          E -> T E x b
          T a1 y a2 | y <= pivot -> T (bigger pivot a2) x b
                    | otherwise  -> T (bigger pivot a1) y (T a2 x b)

partition :: Ord a => a -> SplayHeap a -> (SplayHeap a, SplayHeap a)
partition _ E = (E, E)
partition pivot t@(T a x b)
  | x <= pivot = case b of
       E -> (t, E)
       T b1 y b2 | y <= pivot -> (T (T a x b1) y small, big)
                                 where (small, big) = partition pivot b2
       T b1 y b2              -> (T a x small, T big y b2)
                                 where (small, big) = partition pivot b1
  | otherwise  = case a of
       E -> (E, t)
       T a1 y a2 | y <= pivot -> (T a1 y small, T big x b)
                                 where (small, big) = partition pivot a2
       T a1 y a2              -> (small, T big y (T a2 x b))
                                 where (small, big) = partition pivot a1

instance Heap SplayHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x t = T a x b
    where 
      (a, b) = partition x t

  merge E t = t
  merge (T a x b) t = T (merge ta a) x (merge tb b)
    where
      (ta, tb) = partition x t

  findMin E         = Nothing
  findMin (T E x b) = Just x
  findMin (T a x b) = findMin a

  deleteMin E                 = Nothing
  deleteMin (T E x b)         = Just b
  deleteMin (T (T E x b) y c) = Just $ T b y c
  deleteMin (T (T a x b) y c) = liftA3 T (deleteMin a) (pure x) (pure $ T b y c)
