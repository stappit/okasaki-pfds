module Chap05.Data.BinaryTree where

import Prelude hiding (foldr)
import Test.QuickCheck (Arbitrary(..), sized, choose)
import Control.Applicative (pure, liftA3)
import Data.Foldable

data BinaryTree a = E
                  | T a (BinaryTree a) (BinaryTree a)
                  deriving (Show, Eq)

instance Functor BinaryTree where
  fmap _ E = E
  fmap f (T x a b) = T (f x) (fmap f a) (fmap f b)

instance Foldable BinaryTree where
  foldr _ z E = z
  foldr f z (T x a b) = foldr f (f x $ foldr f z b) a

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = sized arb
    where
        arb 0 = pure E
        arb n = do
            p <- choose (0, n-1)
            x <- arbitrary
            a <- arb p
            b <- arb $ n - p - 1
            return $ T x a b
