module Chap02.Data.BinaryTree where

import Prelude hiding (foldr)
import Test.QuickCheck (Arbitrary(..), sized)
import Control.Applicative (pure, liftA3)
import Data.Foldable

data BinaryTree a = E
                  | T (BinaryTree a) a (BinaryTree a)
                  deriving (Show, Eq)

instance Functor (BinaryTree) where
  fmap _ E = E
  fmap f (T l x r) = T (fmap f l) (f x) (fmap f r)

instance Foldable BinaryTree where
  foldr _ z E = z
  foldr f z (T l x r) = foldr f (f x $ foldr f z r) l

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = sized arbTree
    where
        arbTree 0 = pure E
        arbTree n = 
            let p = (n-1) `div` 2
                q = n - 1 - p
            in  liftA3 T (arbTree p) arbitrary (arbTree q)

inOrderTraversal :: BinaryTree a -> [a]
inOrderTraversal = foldr (:) []

isCompleteTo :: Eq a => Int -> BinaryTree a -> Bool
isCompleteTo d E
  | d == (-1)          = True
  | otherwise          = False
isCompleteTo d (T E _ E) 
  | d == 0             = True
  | otherwise          = False
isCompleteTo d (T l _ r) = isCompleteTo d' l && isCompleteTo d' r
    where
      d' = d - 1

size :: Foldable f => f a -> Int
size = foldr (const (+1)) 0

getBalance :: BinaryTree a -> Int
getBalance E         = -1
getBalance (T l _ r) = size l - size r

isBalanced :: Int -> Bool
isBalanced n = abs n < 2
