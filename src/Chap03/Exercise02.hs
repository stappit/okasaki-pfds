module Chap03.Exercise02 where

import qualified Chap03.Data.Heap as H
import Chap03.Data.LeftistHeap

import Data.Monoid

insert :: (Ord a, Monoid r, Enum r, Ord r) => a -> LeftistHeap r a -> LeftistHeap r a
insert x (LH E) = makeT x H.empty H.empty
insert x (LH (T _ y l r))
  | x < y       = makeT x (LH l) $ insert y (LH r) 
  | otherwise   = makeT y (LH l) $ insert x (LH r)
