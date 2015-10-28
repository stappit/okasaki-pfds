module Chap05.Exercise07 (splaySort) where

import Chap05.Data.SplayHeap

splaySort :: Ord a => [a] -> [a]
splaySort = toList . foldr insert E
