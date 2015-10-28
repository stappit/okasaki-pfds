module Chap02.Exercise03.Test where

import Chap02.Exercise03
import Chap02.Data.UnbalancedSet hiding (insert, member)
import Data.Foldable (toList)

prop_Ex3InsertBST :: Int -> UnbalancedSet Int -> Bool
prop_Ex3InsertBST x s = isSorted . toList $ insert x s
    where
        isSorted []           = True
        isSorted [_]          = True
        isSorted (a:l@(b:_))  = a < b && isSorted l

