module Chap02.Exercise05.Test where

import Test.QuickCheck.Modifiers
import Test.HUnit hiding (Test)

import Chap02.Exercise05
import Chap02.Data.UnbalancedSet

test_CompleteIsComplete :: Assertion
test_CompleteIsComplete = all (\d -> isCompleteTo d $ complete () d) [0..15] @=? True

prop_Sized :: NonNegative Int -> Bool
prop_Sized (NonNegative n) = size (balance () n) == n

prop_Balanced :: NonNegative Int -> Bool
prop_Balanced (NonNegative n) = isBalanced . getBalance $ balance () n
