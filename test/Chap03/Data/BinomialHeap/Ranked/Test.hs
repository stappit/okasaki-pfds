module Chap03.Data.BinomialHeap.Ranked.Test where

import Util
import Chap03.Data.Heap 
import Chap03.Data.BinomialHeap.Ranked 
import Chap03.Data.BinomialTree.Ranked 

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary)

prop_HeapOrder :: (Heap h, Ord a) =>
                   (h a -> BinomialHeap a) ->
                   h a -> Bool
prop_HeapOrder f h = all isHeapOrdered ts 
  where
    ts = unBH . f $ h
    isHeapOrdered (BT _ x cs) = all ((x <=) . root) cs && all isHeapOrdered cs

prop_Rank :: (Heap h, Ord a) =>
             (h a -> BinomialHeap a) ->
             h a -> Bool
prop_Rank f = all isCorrectRank . unBH . f
  where
    isCorrectRank (BT r _ cs) = r == length cs && all isCorrectRank cs
             
testBinomialInv :: ( Heap h
                   , Ord a
                   , Arbitrary (h a)
                   , Arbitrary a
                   , Show (h a)
                   , Show a
                   ) =>
                   (h a -> BinomialHeap a) -> [Test]
testBinomialInv f =
 [ testProperty "Heap ordered" $ prop_HeapOrder f
 , testProperty "Correct ranking" $ prop_Rank f
 ]
