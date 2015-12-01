module Chap06.Data.BottomUpMergeSort where

import Chap06.Data.Sortable

data MergeSort a = BUMS Int [[a]]
                 deriving Show

mrg :: Ord a => [a] -> [a] -> [a]
mrg [] ys = ys
mrg xs [] = xs
mrg xs@(x:xs') ys@(y:ys')
  | x <= y    = x : mrg xs' ys
  | otherwise = y : mrg xs ys'

instance Sortable MergeSort where
  empty = BUMS 0 []

  add x (BUMS size segs) =
    let addSeg seg' segs' size'
          | size' `mod` 2 == 0 = seg' : segs'
          | otherwise          = addSeg (mrg seg' $ head segs') (tail segs') (size' `div` 2)
    in  BUMS (size+1) $ addSeg [x] segs size

  sort (BUMS _ segs) = foldl mrg [] segs
