module Chap05.Data.Queue where

class Deque q where
  empty   :: q a
  isEmpty :: q a -> Bool
  snoc    :: q a ->   a -> q a
  head    :: q a -> Maybe a
  tail    :: q a -> Maybe (q a)
