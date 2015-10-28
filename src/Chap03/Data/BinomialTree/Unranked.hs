module Chap03.Data.BinomialTree.Unranked where

import Data.Foldable
import Prelude hiding (foldr)

data MultiwayTree a = Node a [MultiwayTree a]
                    deriving Show

root' :: MultiwayTree a -> a
root' (Node x _) = x

instance Foldable MultiwayTree where
  foldr f z (Node x cs) = f x $ foldr (flip $ foldr f) z cs

data BinomialTree a = BT 
                      { rank :: Int
                      , root :: a
                      , kids :: [MultiwayTree a]
                      }
                    deriving (Show)

instance Foldable BinomialTree where
  foldr f z (BT _ x cs) = f x $ foldr (flip $ foldr f) z cs

demote :: BinomialTree a -> MultiwayTree a
demote (BT _ x cs) = Node x cs

promote :: Int -> MultiwayTree a -> BinomialTree a
promote r (Node a cs) = BT r a cs

link :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
link (BT r x1 c1) (BT _ x2 c2)
  | x1 < x2   = BT (r+1) x1 (Node x2 c2 : c1)
  | otherwise = BT (r+1) x2 (Node x1 c1 : c2)

