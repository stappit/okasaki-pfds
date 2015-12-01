module Chap06.Data.SizedHeap where

import Chap03.Data.Heap (Heap(..))
import Data.Functor ((<$>))

data SizedHeap h a = SH Int (h a)

instance (Heap h) => Heap (SizedHeap h) where
  empty = SH 0 empty

  isEmpty (SH 0 _) = True
  isEmpty _        = False

  insert x (SH n ha) = SH (n+1) $ insert x ha

  merge (SH m h1) (SH n h2) = SH (m+n) $ merge h1 h2

  findMin (SH _ h) = findMin h

  deleteMin (SH n h) = SH (n-1) <$> deleteMin h
