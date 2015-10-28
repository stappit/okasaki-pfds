module Chap05.Data.Deque where

class Deque q where
  empty   :: q a
  isEmpty :: q a -> Bool
  -- front operations
  cons    ::   a -> q a -> q a
  head    :: q a -> Maybe a
  tail    :: q a -> Maybe (q a)
  -- rear operations
  snoc    :: q a ->   a -> q a
  last    :: q a -> Maybe a
  init    :: q a -> Maybe (q a)
