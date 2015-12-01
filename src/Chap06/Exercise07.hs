module Chap06.Exercise07 where

import Chap06.Data.BottomUpMergeSort

import Data.List

extract :: (Ord a) => Int -> MergeSort a -> [a]
extract k _             | k <= 0 = []
extract k (BUMS k' xss) | k >= k' = concat xss
extract k (BUMS _ xss) = go k xss
  where
    go _ []           = []
    go k _   | k <= 0 = []
    go k xss          = 
      let m = minimum $ [ x | (x:_) <- xss ]
          rest = snd $ foldr chk (False, []) xss
          chk [] yss = yss
          chk xs@(x:xs') (b, yss)
            | b || x /= m = (b, xs:yss)
            | x == m      = (True, xs':yss)
            | otherwise   = (True, yss)
      in  m : go (k-1) rest
