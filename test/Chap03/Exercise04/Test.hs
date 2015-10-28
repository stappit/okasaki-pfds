module Chap03.Exercise04.Test where

import Util
import Chap03.Data.LeftistHeap
import Chap03.Data.LeftistHeap4

prop_WBRSpineIsBdd :: LeftistHeap4 Int -> Bool
prop_WBRSpineIsBdd t = r <= floor (log2 (1 + size t))
    where 
        r = rspine . unLH . unC3E4 $ t
        rspine :: TaggedBinaryTree r a -> Int
        rspine E = 0
        rspine (T _ _ _ b) = 1 + rspine b
        log2 :: Int -> Double
        log2 = logBase 2.0 . toEnum
