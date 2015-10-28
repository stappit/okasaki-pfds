module Chap03.Exercise10 where

import Chap03.Data.RedBlackTree 

lbalance :: Colour -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance col l x r = T col l x r

rbalance :: Colour -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l x r = T c l x r

llbalance :: Colour -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
llbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
llbalance c l x r = T c l x r

lrbalance :: Colour -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
lrbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lrbalance c l x r = T c l x r

rlbalance :: Colour -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
rlbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rlbalance c l x r = T c l x r

rrbalance :: Colour -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
rrbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rrbalance c l x r = T c l x r

insert :: Ord a => a -> RedBlackTree a -> RedBlackTree a
insert x t = 
  let ins :: Ord a => a -> RedBlackTree a -> RedBlackTree a
      ins x' E = T R E x' E
      ins x' t'@(T col a' y' b')
        | x' < y' = case a' of
                    E -> T col (ins x' a') y' b'
                    T _ _ z _ | x' < z -> llbalance col (ins x' a') y' b'
                              | x' > z -> lrbalance col (ins x' a') y' b'
                    _ -> t'
        | x' > y' = case b' of
                    E -> T col a' y' (ins x' b')
                    T _ _ z _ | x' < z -> rlbalance col a' y' (ins x' b')
                              | x' > z -> rrbalance col a' y' (ins x' b')
                    _ -> t'
        | otherwise = t'
      T _ a y b = ins x t
  in  T B a y b -- root node always black

