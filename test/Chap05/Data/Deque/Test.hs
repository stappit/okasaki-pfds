module Chap05.Data.Deque.Test where

import Chap05.Data.Deque
import Data.Foldable
import Prelude hiding (foldr, foldl)

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary)

prop_ConsOrder :: (Deque q, Foldable q, Eq a) => 
                  q a -> 
                  [a] -> Bool
prop_ConsOrder q xs = xs == (toList $ foldr cons (empty `asTypeOf` q) xs)

prop_SnocOrder :: (Deque q, Foldable q, Eq a) => 
                  q a -> 
                  [a] -> Bool
prop_SnocOrder q xs = xs == (toList $ foldl snoc (empty `asTypeOf` q) xs)

testDeque :: ( Deque q
             , Foldable q
             , Arbitrary (q a)
             , Arbitrary a
             , Show (q a)
             , Show a
             , Eq a
             ) => 
             q a -> [Test]
testDeque q =
  [
    testProperty "Deque 1: cons preserves deque invariant" $ prop_ConsOrder q
  , testProperty "Deque 2: snoc preserves deque invariant" $ prop_SnocOrder q
  ]
