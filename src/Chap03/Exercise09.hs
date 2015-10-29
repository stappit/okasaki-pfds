module Chap03.Exercise09 where

import Chap03.Data.RedBlackTree 

fromOrdList :: Ord a => [a] -> RedBlackTree a
fromOrdList xs =
   let go :: Int -> [a] -> (RedBlackTree a, Int, [a])
       go 0 xs'   = (E, -1, xs')
       go 1 (x:xs')  = (T B E x E, 0, xs')
       go len xs' = 
         let mid = len `div` 2
             (p, q) = (mid, len - mid - 1)
             (l, bdl, (x:xs'')) = go p xs'
             (r, bdr, xs''')    = go q xs''
         in  (T B (if bdl == bdr then l else paint R l) x r, bdr + 1, xs''')
       (t, _, _) = go (length xs) xs
   in  t
