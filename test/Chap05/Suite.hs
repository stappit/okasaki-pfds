module Chap05.Suite where

import Test.Framework                       (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Util

import Chap03.Data.Heap.Test         (testHeap)
import Chap05.Data.Deque.Test        (testDeque)
import Chap05.Data.BatchedDeque.Test (testBDInv)
import Chap05.Data.SplayHeap.Test    (testSplayInv)

import qualified Chap05.Data.BatchedDeque as BD

import qualified Chap05.Data.SplayHeap            as SH
import qualified Chap05.Data.SplayHeap.Exercise04 as Ex4

import qualified Chap05.Data.PairingHeap            as PH
import qualified Chap05.Data.PairingHeap.Exercise08 as Ex8

import Chap05.Exercise07 (splaySort)

-- ex 1 tested as a deque
-- ex 2: no tests
-- ex 3: no tests
-- ex 4 tested as a heap
-- ex 5: no tests
-- ex 6: no tests
-- ex 7: defined below
-- ex 8 tested as a heap
-- ex 9: no tests

chap05Suite :: Test
chap05Suite = testGroup "Chapter 5" $ 
  [ 
    testGroup "Splay heap (default implementation)" $
         testHeap (undefined :: SH.SplayHeap Int)
      ++ testSplayInv
  , testGroup "Pairing heap (default implementation)" $
         testHeap (undefined :: PH.PairingHeap Int)
  , testGroup "Exercise 1" $
         testDeque (undefined :: BD.BatchedDeque Int)
      ++ testBDInv (id :: BD.BatchedDeque Int -> BD.BatchedDeque Int)
  , testGroup "Exercise 4: Splay heap" $
         testHeap (undefined :: Ex4.SplayHeap Int)
  , testGroup "Exercise 7" $
         [testProperty "sorts list" (isSorted . splaySort :: [Int] -> Bool)]
  , testGroup "Exercise 8: Pairing heap" $
         testHeap (undefined :: Ex8.PairingHeap Int)
  ]

