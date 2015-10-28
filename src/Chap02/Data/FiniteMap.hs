{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleInstances #-} 

module Chap02.Data.FiniteMap where

class FiniteMap m k a where
  empty  :: m k a 
  bind   :: k ->     a -> m k a -> m k a
  lookup :: k -> m k a -> Maybe a

