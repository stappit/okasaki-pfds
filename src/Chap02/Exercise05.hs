module Chap02.Exercise05 where

import Chap02.Data.BinaryTree

complete :: a -> Int -> BinaryTree a
complete a d 
  | d < 0     = E
  | otherwise = T s a s 
      where
        s = complete a (d-1)

balance :: a -> Int -> BinaryTree a
balance _ 0 = E
balance a n = T s a (if odd n then s else t)
  where
    (s, t) = create2 a $ (n-1) `div` 2
    create2 :: a -> Int -> (BinaryTree a, BinaryTree a)
    create2 x m
      | m < 0     = (E, E)
      | m == 0    = (E, T E x E)
      | odd m     = let (s', t') = create2 x $ (m-1) `div` 2 in (T s' x s', T s' x t')
      | otherwise = let (s', t') = create2 x $ (m-2) `div` 2 in (T s' x t', T t' x t')

