module Chap03.Suite where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Chap02.Data.Set.Test (testSet)
import Chap03.Data.Heap.Test (testHeap)

import Chap03.Data.RedBlackTree      (RedBlackTree, isRedInv, isBlackInv)
import Chap03.Data.RedBlackTree10    (RedBlackTree10, unC3E10)
import Chap03.Data.RedBlackTree.Test (testRBInv)

import Chap03.Data.LeftistHeap  (LeftistHeap, SpineRank, WeightRank)
import Chap03.Data.LeftistHeap2 (LeftistHeap2, unC3E2)
import Chap03.Data.LeftistHeap4 (LeftistHeap4, unC3E4)
import Chap03.Data.LeftistHeap.Test (testLeftistInv)

import qualified Chap03.Data.BinomialHeap.Ranked      as R (BinomialHeap)
import qualified Chap03.Data.BinomialHeap.Ranked.Test as R (testBinomialInv)

import qualified Chap03.Data.BinomialHeap.Unranked      as U (BinomialHeap)
import qualified Chap03.Data.BinomialHeap.Unranked.Test as U (testBinomialInv)

import Chap03.Exercise07 (ExplicitMin)
import Chap03.Exercise09 (fromOrdList)

import Chap03.Exercise01.Test
-- ex 2 tested as a heap
-- ex 3 has no tests
import Chap03.Exercise04.Test
-- ex 4 tested as a heap
-- ex 5 tested as a heap
-- ex 6 tested as a heap
-- ex 7 tested as a heap
import Chap03.Exercise08.Test
{-import Chap03.Exercise10.Test-}

chap03Suite :: Test
chap03Suite = testGroup "Chapter 3" $ 
  [ testGroup "Leftist heap (default implementation)" $
         testHeap (undefined :: LeftistHeap SpineRank Int)
      ++ testLeftistInv (id :: LeftistHeap SpineRank Int -> LeftistHeap SpineRank Int)
  , testGroup "Red-black tree (default implementation)" $
         testSet  (undefined :: RedBlackTree Int)
      ++ testRBInv (id :: RedBlackTree Int -> RedBlackTree Int)
      ++ testRBInv (id :: RedBlackTree Int -> RedBlackTree Int)
  , testGroup "Exercise 1" $
      [ testProperty "length right spine <= [log (size + 1)]" 
                     prop_RSpineIsBdd
      ]
  , testGroup "Exercise 2" $
         testHeap (undefined :: LeftistHeap2 Int)
      ++ testLeftistInv (unC3E2 :: LeftistHeap2 Int -> LeftistHeap SpineRank Int)
  -- no tests for ex 3
  , testGroup "Exercise 4" $
      [ testProperty "length right spine <= [log (size + 1)]" 
                      prop_WBRSpineIsBdd
      ]
      ++ testHeap (undefined :: LeftistHeap4 Int)
      ++ testLeftistInv (unC3E4 :: LeftistHeap4 Int -> LeftistHeap WeightRank Int)
  , testGroup "Exercise 5" $
         testHeap (undefined :: R.BinomialHeap Int)
      ++ R.testBinomialInv (id :: R.BinomialHeap Int -> R.BinomialHeap Int)
  , testGroup "Exercise 6" $
         testHeap (undefined :: U.BinomialHeap Int)
      ++ U.testBinomialInv (id :: U.BinomialHeap Int -> U.BinomialHeap Int)
  , testGroup "Exercise 7" $
      testHeap (undefined :: ExplicitMin U.BinomialHeap Int)
  , testGroup "Exercise 8" $
      [ testProperty "depth of red-black tree <= 2[log (size + 1)]" 
                      prop_DepthBounded
      ]
  , testGroup "Exercise 9" $
      [ testProperty "red invariant"   $ isRedInv   . fromOrdList . (\n -> [0..n] :: [Int])
      , testProperty "black invariant" $ isBlackInv . fromOrdList . (\n -> [0..n] :: [Int])
      ]
  , testGroup "Exercise 10" $
         testSet (undefined :: RedBlackTree10 Int)
      ++ testRBInv (unC3E10 :: RedBlackTree10 Int -> RedBlackTree Int)
  ]
