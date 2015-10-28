module Chap03.Data.Heap where

class Heap h where
  empty     :: Ord a => h a
  isEmpty   :: Ord a => h a -> Bool
  insert    :: Ord a =>   a -> h a -> h a
  merge     :: Ord a => h a -> h a -> h a
  findMin   :: Ord a => h a -> Maybe a     -- may be empty
  deleteMin :: Ord a => h a -> Maybe (h a) -- may be empty
