{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chap03.Data.LeftistHeap4 where

import qualified Chap03.Exercise02 as Ex2
import qualified Chap03.Exercise04 as Ex4
import Chap03.Data.Heap  
import Chap03.Data.LeftistHeap

import Control.Applicative (liftA2, pure, (<$>))
import Data.Foldable
import Prelude hiding (foldr)

import Test.QuickCheck (Arbitrary(..), sized)

newtype LeftistHeap4 a = C3E4 {unC3E4 :: LeftistHeap WeightRank a}

instance Show a => Show (LeftistHeap4 a) where
  show (C3E4 t) = show t

instance Foldable LeftistHeap4 where
  foldr f z (C3E4 h) = foldr f z h

instance Heap LeftistHeap4 where
  empty                     = C3E4 empty
  isEmpty         (C3E4 h)  = isEmpty h
  merge (C3E4 h1) (C3E4 h2) = C3E4 $ Ex4.merge h1 h2
  insert      x   (C3E4 h)  = C3E4 $ Ex2.insert x h
  findMin         (C3E4 h)  = findMin h
  deleteMin       (C3E4 h)  = C3E4 <$> deleteMin h

instance (Arbitrary a, Ord a) => Arbitrary (LeftistHeap4 a) where
  arbitrary = sized arbLH
    where
        arbLH 0 = pure empty
        arbLH 1 = liftA2 insert arbitrary (pure empty)
        arbLH n = liftA2 merge (arbLH p) (arbLH q)
            where
                p = n `div` 2
                q = n - p
