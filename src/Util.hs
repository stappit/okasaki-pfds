module Util where

import Data.Foldable
import Prelude hiding (foldr)

size :: Foldable t => t a -> Int
size = foldr (const (+1)) 0

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:l@(y:_)) = x <= y && isSorted l
