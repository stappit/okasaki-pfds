module Chap05.Exercise04 (smaller) where

import Chap05.Data.SplayHeap

smaller :: Ord a => a -> SplayHeap a -> SplayHeap a
smaller _ E = E
smaller pivot (T a x b)
  | pivot < x = smaller pivot a
  | otherwise = case b of
          E -> T a x E
          T b1 y b2 | pivot < y -> T a x (smaller pivot b1)
                    | otherwise -> T (T a x b1) y (smaller pivot b2)
