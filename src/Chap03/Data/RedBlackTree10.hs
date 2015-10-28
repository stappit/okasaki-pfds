{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chap03.Data.RedBlackTree10 where

import qualified Chap03.Exercise10 as Ex10
import Chap02.Data.Set  
import Chap03.Data.RedBlackTree

import Control.Applicative (liftA2, pure)
import Data.Foldable
import Prelude hiding (foldr)

import Test.QuickCheck (Arbitrary(..), sized)

newtype RedBlackTree10 a = C3E10 {unC3E10 :: RedBlackTree a}

instance Show a => Show (RedBlackTree10 a) where
  show (C3E10 t) = show t

instance Foldable RedBlackTree10 where
  foldr f z (C3E10 h) = foldr f z h

instance Ord a => Set RedBlackTree10 a where
  empty              = C3E10 empty
  member x (C3E10 t) = member x t
  insert x (C3E10 t) = C3E10 $ Ex10.insert x t

instance (Arbitrary a, Ord a) => Arbitrary (RedBlackTree10 a) where
  arbitrary = sized arb
    where
        arb 0 = pure empty
        arb n = liftA2 insert arbitrary $ arb (n - 1)
