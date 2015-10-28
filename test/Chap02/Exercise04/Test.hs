module Chap02.Exercise04.Test where

import Chap02.Exercise04 (insert)
import Chap02.Data.UnbalancedSet hiding (insert, member)
import Data.Foldable (toList)

prop_Ex4InsertBST :: Int -> UnbalancedSet Int -> Bool
prop_Ex4InsertBST n t = isSorted . toList $ insert n t
    where
        isSorted []           = True
        isSorted [_]          = True
        isSorted (x:l@(y:_))  = x < y && isSorted l

