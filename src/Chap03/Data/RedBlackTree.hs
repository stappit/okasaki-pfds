{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chap03.Data.RedBlackTree 
    ( module Chap03.Data.RedBlackTree
    ) where

import Test.QuickCheck (Arbitrary(..), sized)
import Control.Applicative (liftA2, pure)
import Data.Foldable
import Chap02.Data.Set

data Colour = R | B deriving (Show)

data RedBlackTree a = E
                    | T Colour (RedBlackTree a) a (RedBlackTree a)
                    deriving (Show, Foldable)

paint :: Colour -> RedBlackTree a -> RedBlackTree a
paint _ E           = E
paint c (T _ l x r) = T c l x r

balance :: Colour -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l x r = T c l x r

instance Ord a => Set RedBlackTree a where
  empty = E
  member _ E = False
  member x (T _ l y r)
    | x < y     = member x l
    | x > y     = member x r
    | otherwise = True
  insert x t = 
    let ins :: Ord a => a -> RedBlackTree a -> RedBlackTree a
        ins x' E = T R E x' E
        ins x' t'@(T col a' y' b')
          | x' < y'   = balance col (ins x' a') y' b'
          | x' > y'   = balance col a' y' (ins x' b')
          | otherwise = t'
        T _ a y b = ins x t
    in  T B a y b

depth :: RedBlackTree a -> Int
depth E = -1
depth (T _ l _ r) = 1 + depth l `max` depth r

blackDepth :: RedBlackTree a -> Int
blackDepth E = -1
blackDepth (T R l _ r) = blackDepth l `max` blackDepth r
blackDepth (T B l _ r) = (blackDepth l `max` blackDepth r) + 1

isRedInv :: RedBlackTree a -> Bool
isRedInv E                     = True
isRedInv (T R (T R _ _ _) _ _) = False
isRedInv (T R _ _ (T R _ _ _)) = False
isRedInv (T _ l _ r)           = isRedInv l && isRedInv r

isBlackInv :: RedBlackTree a -> Bool
isBlackInv E           = True
isBlackInv (T _ l _ r) = blackDepth l == blackDepth r

instance (Arbitrary a, Ord a) => Arbitrary (RedBlackTree a) where
  arbitrary = sized arbRBTree
    where
        arbRBTree 0 = pure E
        arbRBTree n = liftA2 insert arbitrary $ arbRBTree (n - 1)
