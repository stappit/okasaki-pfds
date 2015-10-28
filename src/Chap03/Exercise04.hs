module Chap03.Exercise04 where

import Chap03.Data.LeftistHeap
import Data.Monoid

merge :: (Ord a, Monoid r, Enum r, Ord r) => 
         LeftistHeap r a -> LeftistHeap r a -> LeftistHeap r a
merge h (LH E) = h
merge (LH E) h = h
merge h1@(LH (T _ x a1 b1)) h2@(LH (T _ y a2 b2))
  | x < y     = let LH b1' = merge (LH b1) h2 
                    rb1'   = rank (LH b1')
                    ra1    = rank (LH a1)
                 in if ra1 >= rb1'
                    then LH $ T (succ $ ra1 <> rb1') x a1 b1' 
                    else LH $ T (succ $ ra1 <> rb1') x b1' a1
  | otherwise = let LH b2' = merge h1 (LH b2) 
                    rb2'   = rank (LH b2')
                    ra2    = rank (LH a2)
                 in if ra2 >= rb2'
                    then LH $ T (succ $ ra2 <> rb2') y a2 b2' 
                    else LH $ T (succ $ ra2 <> rb2') y b2' a2
