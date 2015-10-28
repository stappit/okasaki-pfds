module Chap03.Exercise08.Test where

import Chap03.Data.RedBlackTree 
import Util

prop_DepthBounded :: RedBlackTree Int -> Bool
prop_DepthBounded t = depth t <= 2 * floor (log2 (1 + size t))
    where
        log2 :: Int -> Double
        log2 = logBase 2.0 . toEnum
