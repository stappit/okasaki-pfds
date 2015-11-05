module Chap05.Data.SplayHeap.Exercise04 where

import qualified Chap05.Exercise04     as Ex4
import qualified Chap05.Data.SplayHeap as S
import Chap03.Data.Heap (Heap(..), arbHeap)

import Data.Functor ((<$>))
import Test.QuickCheck (Arbitrary(..), sized)

newtype SplayHeap a = C5E4 (S.SplayHeap a)
                    deriving (Show)

partition :: Ord a => a -> SplayHeap a -> (S.SplayHeap a, S.SplayHeap a)
partition x (C5E4 h) = (Ex4.smaller x h, S.bigger x h)

instance Heap SplayHeap where
  empty = C5E4 empty

  isEmpty (C5E4 h) = isEmpty h

  insert x h = C5E4 $ S.T a x b
    where
        (a, b) = partition x h

  merge (C5E4 S.E) h = h
  merge h (C5E4 S.E) = h
  merge (C5E4 (S.T a x b)) h = C5E4 $ S.T (merge ta a) x (merge tb b)
    where
        (ta, tb) = partition x h

  findMin (C5E4 h) = findMin h

  deleteMin (C5E4 h) = C5E4 <$> deleteMin h

instance (Ord a, Arbitrary a) => Arbitrary (SplayHeap a) where
  arbitrary = sized arbHeap
