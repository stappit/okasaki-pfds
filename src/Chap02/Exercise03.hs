module Chap02.Exercise03 where

import Chap02.Data.Set ()
import Chap02.Data.UnbalancedSet
import Data.Maybe
import Control.Applicative (liftA3, pure, (<$>))

insert :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
insert x t = fromMaybe t $ BST <$> go x (unBST t)
  where
    go :: Ord a => a -> BinaryTree a -> Maybe (BinaryTree a)
    go y E = Just $ T E y E
    go y (T l z r)
      | y < z     = liftA3 T (go y l) (pure z) (pure r)
      | y > z     = T l z <$> go y r
      | otherwise = Nothing
