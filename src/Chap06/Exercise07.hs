module Chap06.Exercise07 where

import Chap06.Data.MergeSort

extract :: (Ord a) => Int -> MergeSort a -> [a]
extract k _             | k <= 0 = []
extract k (BUMS k' xss) | k <= k' = concat xss
extract k (BUMS _ xss) = go k xss [] Nothing []
  where
    go k _  _   _               out | k <= 0 = out
    go k [] yss (Just (z : zs)) out          = go (k-1) (zs : yss) [] Nothing (z : out)
    go k (xs@(x:_) : xss) yss Nothing out    = go k xss yss (Just xs) out
    go k (xs@(x:_) : xss) yss m@(Just zs@(z:_)) out 
      | x < z                                = go k xss (zs : yss) (Just xs) out
      | otherwise                            = go k xss (xs : yss) m         out
