module Chap03.Data.LeftistHeap.Test where

import Chap03.Data.Heap 
import Chap03.Data.LeftistHeap 

import Data.Monoid

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary)

prop_LeftistInv :: (Heap h, Ord a, Monoid r, Ord r) =>
                   (h a -> LeftistHeap r a) ->
                   h a -> Bool
prop_LeftistInv f = isLeftistInv . f
  where
    isLeftistInv (LH E)           = True
    isLeftistInv (LH (T r _ a b)) = rank (LH a) >= rank (LH b)

prop_HeapOrder :: (Heap h, Ord a) =>
                   (h a -> LeftistHeap r a) ->
                   h a -> Bool
prop_HeapOrder f = isHeapOrdered . unLH . f
  where
    isHeapOrdered E = True
    isHeapOrdered (T _ x E E) = True
    isHeapOrdered (T _ x (T _ y a1 a2) E) = x <= y && isHeapOrdered a1 && isHeapOrdered a2
    isHeapOrdered (T _ x E (T _ z b1 b2)) = x <= z && isHeapOrdered b1 && isHeapOrdered b2
    isHeapOrdered (T _ x (T _ y a1 a2) (T _ z b1 b2)) = 
         x <= y && x <= z
      && isHeapOrdered a1 && isHeapOrdered a2
      && isHeapOrdered b1 && isHeapOrdered b2
             
testLeftistInv :: ( Heap h
                  , Ord a
                  , Arbitrary (h a)
                  , Arbitrary a
                  , Show (h a)
                  , Show a
                  , Monoid r
                  , Ord r
                  ) =>
                  (h a -> LeftistHeap r a) -> [Test]
testLeftistInv f =
 [ testProperty "Leftist invariant" $ prop_LeftistInv f
 , testProperty "Heap ordered" $ prop_HeapOrder f
 ]
