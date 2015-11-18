module Chap07.Exercise04 where

import Chap07.Data.BinomialHeap 
    (
      Tree(..)
    , Heap
    , listToStream
    , Digit(..)
    , insTree
    )

mrgWithList :: [Tree a] -> Heap a -> Heap a
mrgWithList ts1 [] = listToStream $ map One ts1
mrgWithList [] ds2 = ds2
mrgWithList (t : t1) (Zero : ds2) = One t : mrgWithList t1 ds2
mrgWithList (t1 : ts1) (One t2 : ds2) = Zero : insTree (link t1 t2) (mrgWithList ts1 ds2)

