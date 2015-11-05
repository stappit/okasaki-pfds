{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 

module Chap02.Data.Set.Test (testSet) where

import Chap02.Data.Set
import Test.QuickCheck (Arbitrary(..), Property, (==>))
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

prop_InsertInsertsAs :: Set s a => 
                        s a ->
                        a -> s a -> Property
prop_InsertInsertsAs _ x s = not (member x s) ==> member x (insert x s)

prop_EmptyIsEmptyAs :: Set s a => 
                     s a ->
                     a -> Bool
prop_EmptyIsEmptyAs s x = not . member x $ empty `asTypeOf` s

testSet :: ( Set s a
           , Arbitrary (s a)
           , Arbitrary a
           , Show (s a)
           , Show a
           ) => 
           s a -> [Test]
testSet s = 
  [ 
    testProperty "set 1: x ∉ s ⇒ x ∈ s ∪ {x}" $
                 prop_InsertInsertsAs s
  , testProperty "set 2: Ø has no elements" $
                 prop_EmptyIsEmptyAs s
  ]
