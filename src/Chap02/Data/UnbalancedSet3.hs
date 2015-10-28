{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chap02.Data.UnbalancedSet3 where

import           Chap02.Data.Set
import           Chap02.Data.UnbalancedSet
import qualified Chap02.Exercise02                 as Ex2 (member)
import qualified Chap02.Exercise03                 as Ex3 (insert)

import Data.Foldable
import Prelude hiding (foldr)

import Test.QuickCheck (Arbitrary(..), sized)
import Control.Applicative (liftA2, pure)

newtype UnbalancedSet3 a = C2E3 {unC2E3 :: UnbalancedSet a}
                         deriving Show

instance Foldable UnbalancedSet3 where
  foldr f z (C2E3 t) = foldr f z t

instance Ord a => Set UnbalancedSet3 a where
  empty    = C2E3 empty
  member x = Ex2.member x . unC2E3
  insert x = C2E3 . Ex3.insert x . unC2E3

instance (Arbitrary a, Ord a) => Arbitrary (UnbalancedSet3 a) where
  arbitrary = sized arbUSet
    where
        arbUSet 0 = pure empty
        arbUSet n = liftA2 insert arbitrary $ arbUSet (n-1)

