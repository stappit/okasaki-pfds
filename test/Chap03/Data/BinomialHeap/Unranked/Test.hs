module Chap03.Data.BinomialHeap.Unranked.Test where

import Util
import Chap03.Data.Heap 
import Chap03.Data.BinomialHeap.Unranked 
import Chap03.Data.BinomialTree.Unranked 

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary)

prop_HeapOrder :: (Heap h, Ord a) =>
                   (h a -> BinomialHeap a) ->
                   h a -> Bool
prop_HeapOrder f h = all isHeapOrdered ts
  where
    ts = unBH . f $ h
    isHeapOrdered  (BT _ x cs) = all ((x <=) . root') cs && all isHeapOrdered' cs
    isHeapOrdered' (Node x cs) = all ((x <=) . root') cs && all isHeapOrdered' cs

prop_Rank :: (Heap h, Ord a) =>
             (h a -> BinomialHeap a) ->
             h a -> Bool
prop_Rank f h = isSorted (fmap rank ts) && all (\t -> isCorrectRank (rank t) t) ts
  where
    ts = unBH . f $ h
    isCorrectRank  r (BT _ _ cs) = r == length cs && and (zipWith isCorrectRank' [(r-1),(r-2)..0] cs)
    isCorrectRank' r (Node _ cs) = r == length cs && and (zipWith isCorrectRank' [(r-1),(r-2)..0] cs)
             
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

