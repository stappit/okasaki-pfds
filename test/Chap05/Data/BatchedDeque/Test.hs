module Chap05.Data.BatchedDeque.Test (testBDInv) where

import Chap05.Data.Deque
import Chap05.Data.BatchedDeque

import Prelude hiding (tail, init)

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary)

prop_BDConsInv :: (Deque q) => 
                  (q a -> BatchedDeque a) ->
                  a -> q a -> Bool
prop_BDConsInv f x = isBatchedDequeInv . f . cons x

prop_BDSnocInv :: (Deque q) => 
                  (q a -> BatchedDeque a) ->
                  a -> q a -> Bool
prop_BDSnocInv f x = isBatchedDequeInv . f . (flip snoc) x

prop_BDTailInv :: (Deque q) => 
                  (q a -> BatchedDeque a) ->
                  q a -> Bool
prop_BDTailInv f = maybe True (isBatchedDequeInv . f) . tail

prop_BDInitInv :: (Deque q) => 
                  (q a -> BatchedDeque a) ->
                  q a -> Bool
prop_BDInitInv f = maybe True (isBatchedDequeInv . f) . init

testBDInv :: ( 
               Deque q
             , Arbitrary (q a)
             , Arbitrary a
             , Show (q a)
             , Show a
             ) =>
             (q a -> BatchedDeque a) -> [Test]

testBDInv f =
 [
   testProperty "cons preserves batched deque invariant" $ prop_BDConsInv f
 , testProperty "snoc preserves batched deque invariant" $ prop_BDSnocInv f
 , testProperty "tail preserves batched deque invariant" $ prop_BDTailInv f
 , testProperty "init preserves batched deque invariant" $ prop_BDInitInv f
 ]
