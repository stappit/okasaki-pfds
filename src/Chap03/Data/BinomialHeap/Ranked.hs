{-# LANGUAGE DeriveFoldable #-}

module Chap03.Data.BinomialHeap.Ranked where

import Chap03.Data.BinomialTree.Ranked
import Chap03.Data.Heap (Heap(..), arbHeap)
import Test.QuickCheck (Arbitrary(..), sized)

import Data.Foldable
import Data.Functor

newtype BinomialHeap a = BH {unBH :: [BinomialTree a]}
                       deriving (Show, Foldable)

insTree :: Ord a => BinomialTree a -> BinomialHeap a -> BinomialHeap a
insTree t (BH [])        = BH [t]
insTree t1 (BH ts@(t2:ts'))
  | rank t1 < rank t2 = BH $ t1 : ts
  | otherwise         = insTree (link t1 t2) (BH ts')

removeMinTree :: Ord a => BinomialHeap a -> Maybe (BinomialTree a, BinomialHeap a)
removeMinTree (BH [])  = Nothing
removeMinTree (BH [t]) = Just (t, BH [])
removeMinTree (BH (t:ts))
  | root t < root t'  = Just (t, BH ts)
  | otherwise         = Just (t', BH $ t : unBH ts')
      where
        Just (t', ts') = removeMinTree $ BH ts

bhFromList :: [BinomialTree a] -> BinomialHeap a
bhFromList = BH . reverse

instance Heap BinomialHeap where
  empty = BH []
  isEmpty (BH []) = True
  isEmpty _       = False
  insert x = insTree (BT 0 x [])
  merge h (BH [])  = h
  merge (BH []) h  = h
  merge h1@(BH (t1:ts1')) h2@(BH (t2:ts2'))
    | rank t1 < rank t2 = BH $ t1 : unBH (merge (BH ts1') h2)
    | rank t1 > rank t2 = BH $ t2 : unBH (merge h1 (BH ts2'))
    | otherwise         = insTree (link t1 t2) $ merge (BH ts1') (BH ts2')
  findMin h = root . fst <$> removeMinTree h
  deleteMin (BH []) = Nothing
  deleteMin h       = Just $ merge (bhFromList ts1) h'
    where
      Just (BT _ _ ts1, h') = removeMinTree h

instance (Ord a, Arbitrary a) => Arbitrary (BinomialHeap a) where
  arbitrary = sized arbHeap
