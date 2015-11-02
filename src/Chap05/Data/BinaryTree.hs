module Chap05.Data.BinaryTree where

import Prelude hiding (foldr)
import Test.QuickCheck (Arbitrary(..), sized)
import Control.Applicative (pure, liftA3)
import Data.Foldable

data BinaryTree a = E
                  | T a (BinaryTree a) (BinaryTree a)
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
