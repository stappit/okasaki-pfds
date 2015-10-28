module Chap02.Suite where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Chap02.Exercise01.Test
import Chap02.Exercise05.Test
{-import Chap02.Exercise06.Test-}

import Chap02.Data.UnbalancedSet  (UnbalancedSet)
import Chap02.Data.UnbalancedSet2 (UnbalancedSet2, unC2E2)
import Chap02.Data.UnbalancedSet3 (UnbalancedSet3, unC2E3)
import Chap02.Data.UnbalancedSet4 (UnbalancedSet4, unC2E4)

import Chap02.Data.Set.Test (testSet)
import Chap02.Data.UnbalancedSet.Test (testBSTInv)

chap02Suite :: Test
chap02Suite = testGroup "Chapter 2" $ 
     [ testGroup "Unbalanced set (default implementation)" $ 
            testSet (undefined :: UnbalancedSet Int)
         ++ testBSTInv (id :: UnbalancedSet Int -> UnbalancedSet Int)
     , testGroup "Exercise 1" $
         [ testCase "suffixes [1, 2, 3, 4] = [[1, 2, 3, 4], [2, 3, 4], [3, 4], [4], []]" test_suffixes
         ]
     , testGroup "Exercise 2" $
            testSet (undefined :: UnbalancedSet2 Int)
         ++ testBSTInv (unC2E2 :: UnbalancedSet2 Int -> UnbalancedSet Int)
     , testGroup "Exercise 3" $
            testSet (undefined :: UnbalancedSet3 Int)
         ++ testBSTInv (unC2E3 :: UnbalancedSet3 Int -> UnbalancedSet Int)
     , testGroup "Exercise 4" $
            testSet (undefined :: UnbalancedSet4 Int)
         ++ testBSTInv (unC2E4 :: UnbalancedSet4 Int -> UnbalancedSet Int)
     , testGroup "Exercise 5" $
         [ testCase     "complete yields a complete tree" test_CompleteIsComplete
         , testProperty "balance yields trees of correct size" prop_Sized
         , testProperty "balance yields balanced trees" prop_Balanced
         ]
     ]
