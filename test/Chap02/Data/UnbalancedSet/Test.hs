module Chap02.Data.UnbalancedSet.Test (testBSTInv) where

import Util
import Data.Foldable
import Chap02.Data.UnbalancedSet (UnbalancedSet)

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary)

prop_BSTInv :: (Foldable t, Ord a) => 
               (t a -> UnbalancedSet a) ->
               t a -> Bool
prop_BSTInv f = isSorted . toList . f

testBSTInv :: ( Foldable t
              , Ord a
              , Arbitrary (t a)
              , Arbitrary a
              , Show (t a)
              , Show a
              ) =>
              (t a -> UnbalancedSet a) -> [Test]
testBSTInv f =
  [ testProperty "BST invariant" $ prop_BSTInv f
  ]
