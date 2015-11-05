module Chap05.Data.BatchedDeque where

import Test.QuickCheck (Arbitrary(..), sized)

import Chap05.Data.Deque
import Control.Applicative ((<$>), (<*>), liftA2, pure)
import Data.Foldable
import Prelude hiding (head, tail, foldr)

data BatchedDeque a = Q Int [a] Int [a]
                    deriving (Show)

instance Foldable BatchedDeque where
  foldr g z (Q _ f _ r) = foldr g z $ f ++ reverse r

isBatchedDequeInv :: BatchedDeque a -> Bool
isBatchedDequeInv (Q 0 _ lenr _) | lenr > 1 = False
isBatchedDequeInv (Q lenf _ 0 _) | lenf > 1 = False
isBatchedDequeInv _                         = True

turn :: BatchedDeque a -> BatchedDeque a
turn (Q lenf f lenr r) = Q lenr r lenf f

check :: Int -> [a] -> Int -> [a] -> BatchedDeque a
check lenf f@(_:_:_) _ [] = Q half f' (lenf - half) (reverse r')
  where
    half = lenf `div` 2
    (f', r') = splitAt half f
check _ [] lenr r@(_:_:_) = turn $ check lenr r 0 []
check lenf f lenr r       = Q lenf f lenr r

instance Deque BatchedDeque where
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

instance (Arbitrary a) => Arbitrary (BatchedDeque a) where 
  arbitrary = sized arb
    where
        arb 0 = pure empty
        arb n = ins <*> arbitrary <*> (arb $ n-1)
        ins = do
            b <- arbitrary
            if b then return cons else return $ flip snoc
