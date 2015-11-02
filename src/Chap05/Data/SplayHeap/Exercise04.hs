module Chap05.Data.SplayHeap.Exercise04

import Chap05.Exercise04     as Ex4
import Chap05.Data.SplayHeap as S
import Chap03.Data.Heap

import Data.Functor ((<$>))

newtype SplayHeap a = C5E4 (S.SplayHeap a)

partition :: Ord a => a -> S.SplayHeap a -> (S.SplayHeap a, S.SplayHeap a)
partition x (C5E4 h) = (Ex4.smaller x h, S.bigger x h)

instance Heap SplayHeap where
  empty = C5E4 empty

  isEmpty (C5E4 b) = isEmpty h

  insert x (C5E4 h) = C5E4 $ T a x b
    where
        (a, b) = partition x h

  merge (C5E4 E) h = h
  merge h (C5E4 E) = h
  merge (C5E4 (T a x b)) h = C5E4 $ T (merge ta a) x (merge tb b)
    where
        (ta, tb) = partition x h

  findMin (C5E4 h) = findMin h

  deleteMin (C5E4 h) = C5E4 <$> deleteMin h
