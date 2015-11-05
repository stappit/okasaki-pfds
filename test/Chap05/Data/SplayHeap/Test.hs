module Chap05.Data.SplayHeap.Test (testSplayInv) where

import Util
import Chap05.Data.SplayHeap

import Data.Foldable (toList)

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary)

testSplayInv = [testProperty "BST invariant" prop_SplayInv]

prop_SplayInv :: SplayHeap Int -> Bool
prop_SplayInv = isSorted . toList
