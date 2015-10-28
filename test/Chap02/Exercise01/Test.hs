module Chap02.Exercise01.Test where

import Test.HUnit hiding (Test)

import Chap02.Exercise01

test_suffixes :: Assertion
test_suffixes = suffixes ([1, 2, 3, 4] :: [Int]) @?= [[1, 2, 3, 4], [2, 3, 4], [3, 4], [4], []] 

