{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 

module Chap02.Exercise06 where

import Prelude hiding (lookup)
import Chap02.Data.FiniteMap
import Chap02.Data.UnbalancedSet
import qualified Chap02.Data.Set as S

newtype Binding k v = B (k, v)
                    deriving (Show)

key :: Binding k v -> k
key (B (k, _)) = k

value :: Binding k v -> v
value (B (_, v)) = v

instance Eq k => Eq (Binding k v) where
  B (k, _) == B (k', _) = k == k'

instance (Ord k, Eq v) => Ord (Binding k v) where
  B (k, _) <= B (k', _) = k <= k'

newtype UnbalancedMap k v = M (UnbalancedSet (Binding k v))

instance (Ord k, Eq v) => FiniteMap UnbalancedMap k v where
  empty = M S.empty
  bind k v (M t) = M $ S.insert (B (k, v)) t
  lookup _ (M (BST E)) = Nothing
  lookup k (M (BST (T l b r)))
    | k < key b = lookup k $ M (BST l)
    | k > key b = lookup k $ M (BST r)
    | otherwise = Just $ value b

