module Chap02.Exercise04 where

import Chap02.Data.UnbalancedSet
import Data.Maybe
import Control.Applicative (liftA3, pure, (<$>))

insert :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
insert x t = fromMaybe t $ BST <$> go x (unBST t) []
  where

    go :: Ord a => a -> BinaryTree a -> [(BinaryTree a, a)] -> Maybe (BinaryTree a)
    go x' E ls     = mk x' (Just $ T E x' E) ls
    go x' (T l y r) ls
      | x' < y     = mk x' (liftA3 T (go x' l []) (pure y) (pure r)) ls -- 2
      | otherwise = go x' r $ (l, y):ls                       -- 3

    mk :: Eq a => a -> Maybe (BinaryTree a) -> [(BinaryTree a, a)] -> Maybe (BinaryTree a)
    mk x' = foldl (\mr (l, y) -> if x' == y then Nothing else T l y <$> mr)

