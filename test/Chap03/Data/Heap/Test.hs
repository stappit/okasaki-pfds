module Chap03.Data.Heap.Test (testHeap) where

import Chap03.Data.Heap
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

prop_InsertAs :: (Heap h, Ord a) => 
                 h a ->
                 h a -> a -> Bool
prop_InsertAs _ h x = not . isEmpty . insert x $ h 

prop_InsDelAs :: (Heap h, Ord a) => 
                 h a ->
                 a -> Bool
prop_InsDelAs h x = maybe False isEmpty . deleteMin . insert x $ empty `asTypeOf` h 

prop_InsMinAs :: (Heap h, Ord a) =>
                 h a ->
                 a -> Bool
prop_InsMinAs h x = maybe False (x ==) . findMin . insert x $ empty `asTypeOf` h

prop_MinInsIsMinAs :: (Heap h, Ord a) => 
                      h a ->
                      a -> h a -> Bool
prop_MinInsIsMinAs _ x h = findMin (insert x h) == min (Just x) hmin
  where hmin = if isEmpty h then Just x else findMin h

prop_MinMrgIsMinAs :: (Heap h, Ord a) => 
                    h a ->
                    h a -> h a -> Bool
prop_MinMrgIsMinAs _ h1 h2 = 
  findMin (merge h1 h2) == min (findMin h1) (findMin h2)

testHeap :: ( Heap h
            , Ord a
            , Arbitrary (h a)
            , Arbitrary a
            , Show (h a)
            , Show a
            ) => 
            h a -> [Test]
testHeap h = 
  [ testProperty "heap 1: inserting into then deleting from an empty heap gives the empty heap" $
                 prop_InsDelAs h
  , testProperty "heap 2: inserting gives a nonempty heap" $
                 prop_InsertAs h
  , testProperty "heap 3: findMin (insert x empty) gives x" $
                 prop_InsMinAs h
  , testProperty "heap 4: findMin after inserting x is just min of x and findMin" $
                 prop_MinMrgIsMinAs h
  , testProperty "heap 5: findMin of a merge is the min of findMins" $
                 prop_MinInsIsMinAs h
  ]
