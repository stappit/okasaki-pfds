{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chap02.Data.UnbalancedSet 
  ( module Chap02.Data.UnbalancedSet
  , module Chap02.Data.BinaryTree
  ) where

import Chap02.Data.BinaryTree
import Data.Foldable
import Prelude hiding (foldr)

import Chap02.Data.Set
import Test.QuickCheck (Arbitrary(..), sized)
import Control.Applicative (liftA2, pure)

newtype UnbalancedSet a = BST {unBST :: BinaryTree a}
                        deriving (Show, Eq)

instance Foldable UnbalancedSet where
  foldr f z (BST t) = foldr f z t

instance Ord a => Set UnbalancedSet a where
  empty  = BST E

  insert x (BST E) = BST $ T E x E
  insert x t@(BST (T l y r))
    | x < y     = let l' = (unBST . insert x) (BST l) in BST $ T l' y r -- ordering required!
    | x > y     = let r' = (unBST . insert x) (BST r) in BST $ T l  y r' -- ordering required!
    | otherwise = t

  member _ (BST E) = False
  member x (BST (T l y r))
    | x < y     = member x (BST l) -- ordering required!
    | x > y     = member x (BST r) -- ordering required!
    | otherwise = x == y
  
instance (Arbitrary a, Ord a) => Arbitrary (UnbalancedSet a) where
  arbitrary = sized arbUSet
    where
        arbUSet 0 = pure empty
        arbUSet n = liftA2 insert arbitrary $ arbUSet (n-1)

