module Chap05.Exercise08 (toBinary) where

import Chap05.Data.PairingHeap
import qualified Chap05.Data.BinaryTree as B

toBinary :: Ord a => PairingHeap a -> BinTree a
toBinary h = go h []
  where
    go :: Ord a => PairingHeap a -> [PairingHeap a] -> BinTree a
    go E            _      = B.E
    go (T x [])     []     = B.T x B.E B.E
    go (T x [])     (r:rs) = B.T x B.E (go r rs)
    go (T x (c:cs)) []     = B.T x (go c cs) B.E
    go (T x (c:cs)) (r:rs) = B.T x (go c cs) (go r rs)
