module Chap03.Data.RedBlackTree.Test where

import Chap02.Data.Set
import Chap03.Data.RedBlackTree

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary)

prop_RedInv :: (Ord a, Set t a) => 
               (t a -> RedBlackTree a) ->
               a -> t a -> Bool
prop_RedInv f x = isRedInv . f . insert x 

prop_BlackInv :: (Ord a, Set t a) => 
                 (t a -> RedBlackTree a) ->
                 a -> t a -> Bool
prop_BlackInv f x = isBlackInv . f . insert x

testRBInv ::( Set t a
            , Ord a
            , Arbitrary (t a)
            , Arbitrary a
            , Show (t a)
            , Show a
            ) =>
            (t a -> RedBlackTree a) -> [Test]

testRBInv f =
 [
   testProperty "red invariant" $ prop_RedInv f
 , testProperty "black invariant" $ prop_BlackInv f
 ]
