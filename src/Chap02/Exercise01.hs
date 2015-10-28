module Chap02.Exercise01 where

suffixes :: [a] -> [[a]]
suffixes xs = xs : case xs of
                     []       -> []
                     (_ : ys) -> suffixes ys

