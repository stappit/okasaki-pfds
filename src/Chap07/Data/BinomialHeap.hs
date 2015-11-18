module Chap07.Data.BinomialHeap where

import Chap03.Data.Heap (Heap(..), arbHeap)

data Tree a = Node a [Tree a]
            deriving Show

data Digit a = Zero | One (Tree a)
             deriving Show

type Heap a = [Digit a]

insTree :: Digit a -> Heap a -> Heap a
insTree t []            = [One t]
insTree t (Zero : ds)   = One t : ds
insTree t (One t' : ds) = Zero : insTree (link t t') ds

type Schedule a = [Heap a]

data BinomialHeap a = BH (Heap a) Schedule
                    deriving Show

exec :: Schedule -> Schedule
exec ((One t : _) : sched)   = sched
exec ((Zero  : job) : sched) = job : sched

normalise :: Schedule a -> Schedule a
normalise ds@[] = ds
normalise ds@(_ : ds') = normalise ds' : ds

listToStream = id

mrg :: Heap a -> Heap a -> Heap a
mrg ds1 [] = ds1
mrg [] ds2 = ds2
mrg (Zero : ds1) (d : ds2) = d : mrg ds1 ds2
mrg (d : ds1) (Zero : ds2) = d : mrg ds1 ds2
mrg (One t1 : ds1) (One t2 : ds2) = Zero : insTree (link t1 t2) (mrg ds1 ds2)

removeMinTree :: Heap a -> Maybe (Tree a, Heap a)
removeMinTree [] = Nothing
removeMinTree [One t] = (t, [])
removeMinTree (Zero : ds) = (t', Zero : ds')
    where
        (t', ds') = removeMinTree ds
removeMinTree (One t@(Node x _), ds) =
    case removeMinTree ds of
        (t'@(Node x' _), ds') | x <= x'   -> (t, Zero ds)
                              | otherwise -> (t', One t : ds')

instance Heap BinomialHeap where
  empty = BH [] []

  isEmpty (BH [] _) = True
  isEmpty _         = False

  insert x (BH ds sched) = BH ds' (exec . exec $ ds' : sched)
    where 
        ds' = insTree (Node x []) ds

  merge (BH ds1 _) (BH ds2 _) = BH ds []
    where
        ds = normalise $ mrg ds1 ds2

  findMin (BH ds _) = x
    where
        (Node x _, _) = removeMinTree ds

  deleteMin (BH ds _) = BH (normalise ds'') [] 
    where
        (Node x c, ds') = removeMinTree ds
        ds'' = mrg (map One $ rev c) ds'
