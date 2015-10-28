module Chap03.Exercise03 where

import Chap03.Data.Heap
import Chap03.Data.LeftistHeap

fromList :: Ord a => [a] -> LeftistHeap SpineRank a
fromList = mergeAll . fmap (\x -> makeT x empty empty)
  where
    mergeAll :: Ord a => [LeftistHeap SpineRank a] -> LeftistHeap SpineRank a
    mergeAll []  = empty
    mergeAll [h] = h
    mergeAll hs  = mergeAll . mergePairs $ hs

    mergePairs :: Ord a => [LeftistHeap SpineRank a] -> [LeftistHeap SpineRank a]
    mergePairs (h1:h2:rest) = merge h1 h2 : mergePairs rest
    mergePairs hs           = hs
