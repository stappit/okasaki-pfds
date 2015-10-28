module Chap04.Exercise02 where

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = ins x $ sort xs
    where
        ins x [] = [x]
        ins x ys'@(y:ys)
          | x <= y    = x : ys'
          | otherwise = y : ins x ys
