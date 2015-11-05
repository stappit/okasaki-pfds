module Chap05.Exercise07 (splaySort) where

import Chap03.Data.Heap
import Chap05.Data.SplayHeap
import Data.Foldable
import Prelude hiding (foldr)

splaySort :: Ord a => [a] -> [a]
splaySort = toList . foldr insert E
