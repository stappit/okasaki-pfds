{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chap02.Data.UnbalancedSet4 where

import           Chap02.Data.Set
import           Chap02.Data.UnbalancedSet
import qualified Chap02.Exercise02                 as Ex2 (member)
import qualified Chap02.Exercise04                 as Ex4 (insert)

import Data.Foldable
import Prelude hiding (foldr)

import Test.QuickCheck (Arbitrary(..), sized)
import Control.Applicative (liftA2, pure)

newtype UnbalancedSet4 a = C2E4 {unC2E4 :: UnbalancedSet a}
                         deriving Show

instance Foldable UnbalancedSet4 where
  foldr f z (C2E4 t) = foldr f z t

instance Ord a => Set UnbalancedSet4 a where
  empty    = C2E4 empty
  member x = Ex2.member x . unC2E4
  insert x = C2E4 . Ex4.insert x . unC2E4

instance (Arbitrary a, Ord a) => Arbitrary (UnbalancedSet4 a) where
  arbitrary = sized arbUSet
    where
        arbUSet 0 = pure empty
        arbUSet n = liftA2 insert arbitrary $ arbUSet (n-1)

