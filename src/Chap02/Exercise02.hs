{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chap02.Exercise02 where

import Chap02.Data.Set ()
import Chap02.Data.UnbalancedSet

member :: Ord a => a -> UnbalancedSet a -> Bool
member x t = go x t []
  where
    go :: Ord a => a -> UnbalancedSet a -> [a] -> Bool
    go a (BST E) bs       = a `elem` bs
    go a (BST (T l b r)) bs
        | a < b     = go a (BST l) bs
        | otherwise = go a (BST r) (b:bs)
  
