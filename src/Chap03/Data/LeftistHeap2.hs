{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chap03.Data.LeftistHeap2 where

import qualified Chap03.Exercise02 as Ex2
import Chap03.Data.Heap  
import Chap03.Data.LeftistHeap

import Control.Applicative (liftA2, pure, (<$>))
import Data.Foldable
import Prelude hiding (foldr)

import Test.QuickCheck (Arbitrary(..), sized)

newtype LeftistHeap2 a = C3E2 {unC3E2 :: LeftistHeap SpineRank a}

instance Show a => Show (LeftistHeap2 a) where
  show (C3E2 t) = show t

instance Foldable LeftistHeap2 where
  foldr f z (C3E2 h) = foldr f z h

instance Heap LeftistHeap2 where
  empty = C3E2 empty
  isEmpty (C3E2 h) = isEmpty h
  merge (C3E2 h1) (C3E2 h2) = C3E2 $ merge h1 h2
  insert x (C3E2 h) = C3E2 $ Ex2.insert x h
  findMin (C3E2 h) = findMin h
  deleteMin (C3E2 h) = C3E2 <$> deleteMin h

instance (Arbitrary a, Ord a) => Arbitrary (LeftistHeap2 a) where
  arbitrary = sized arbLH
    where
        arbLH 0 = pure empty
        arbLH 1 = liftA2 insert arbitrary (pure empty)
        arbLH n = liftA2 merge (arbLH p) (arbLH q)
            where
                p = n `div` 2
                q = n - p
