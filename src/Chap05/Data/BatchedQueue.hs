module Chap05.Data.BatchedQueue where

import Chap05.Data.Deque
import Data.Functor ((<$>))
import Prelude hiding (head, tail)

data BatchedQueue a = Q Int [a] Int [a]

turn :: BatchedQueue a -> BatchedQueue a
turn (Q lenf f lenr r) = Q lenr r lenf f

check :: Int -> [a] -> Int -> [a] -> BatchedQueue a
check lenf f@(_:_:_) _ [] = Q half f' (lenf - half) (reverse r')
  where
    half = lenf `div` 2
    (f', r') = splitAt half f
check _ [] lenr r@(_:_:_) = turn $ check lenr r 0 []
check lenf f lenr r       = Q lenf f lenr r

instance Deque BatchedQueue where
  empty = Q 0 [] 0 []

  isEmpty (Q _ [] _ []) = True
  isEmpty _             = False

  cons x (Q lenf f lenr r) = check (lenf+1) (x:f) lenr r

  head (Q _ [] _ [])   = Nothing
  head (Q _ [] _ [x])  = Just x
  head (Q _ (x:_) _ _) = Just x

  tail (Q _ [] _ [])          = Nothing
  tail (Q lenf (_:f') lenr r) = Just $ check (lenf-1) f' lenr r
  tail _                      = Just empty

  snoc q x = turn . cons x . turn $ q
  last = head . turn
  init = (turn <$>) . tail . turn
