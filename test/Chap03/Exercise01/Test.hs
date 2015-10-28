module Chap03.Exercise01.Test where

import Util
import Chap03.Data.LeftistHeap

prop_RSpineIsBdd :: LeftistHeap (Rank RightSpine) Int -> Bool
prop_RSpineIsBdd t = (r <=) . floor . log2 . (1+) . size $ t
    where 
        r = fromEnum $ rank t
        log2 :: Int -> Double
        log2 = logBase 2.0 . toEnum

