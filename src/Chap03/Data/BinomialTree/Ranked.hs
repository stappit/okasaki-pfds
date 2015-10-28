module Chap03.Data.BinomialTree.Ranked where

import Data.Foldable
import Prelude hiding (foldr)

data BinomialTree a = BT 
                      { rank :: Int
                      , root :: a
                      , kids :: [BinomialTree a]
                      }
                    deriving (Show)

instance Foldable BinomialTree where
  foldr f z (BT _ x cs) = f x $ foldr (flip $ foldr f) z cs

link :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
link t1@(BT r x1 c1) t2@(BT _ x2 c2)
  | x1 < x2   = BT (r+1) x1 (t2:c1)
  | otherwise = BT (r+1) x2 (t1:c2)

