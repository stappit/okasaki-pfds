{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chap02.Data.UnbalancedSet2 where

import           Chap02.Data.Set
import           Chap02.Data.UnbalancedSet 
import qualified Chap02.Exercise02         as Ex2 (member)

import Data.Foldable
import Prelude hiding (foldr)

import Control.Applicative (liftA2, pure)
import Test.QuickCheck (Arbitrary(..), sized)

newtype UnbalancedSet2 a = C2E2 {unC2E2 :: UnbalancedSet a}
                         deriving (Show)

instance Foldable UnbalancedSet2 where
  foldr f z (C2E2 t) = foldr f z t

instance Ord a => Set UnbalancedSet2 a where
  empty    = C2E2 empty
  insert x = C2E2 . insert x . unC2E2
  member x = Ex2.member x . unC2E2

instance (Arbitrary a, Ord a) => Arbitrary (UnbalancedSet2 a) where
  arbitrary = sized arbUSet
    where
        arbUSet 0 = pure empty
        arbUSet n = liftA2 insert arbitrary $ arbUSet (n-1)
