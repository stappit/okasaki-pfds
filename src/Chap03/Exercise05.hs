module Chap03.Exercise05 where

import Chap03.Data.BinomialHeap.Ranked
import Chap03.Data.BinomialTree.Ranked 

findMin :: Ord a => BinomialHeap a -> Maybe a
findMin (BH [])     = Nothing
findMin (BH (t:ts)) = myMin (root t) (findMin $ BH ts)
  where
    myMin :: Ord a => a -> Maybe a -> Maybe a
    myMin a Nothing  = Just a
    myMin a (Just b) = Just (min a b)

