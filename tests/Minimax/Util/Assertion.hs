{-
    Module: Assertion.

    Some useful functions to write tests (only for integers).
-}

module Util.Assertion where 

import Test.HUnit

-- Assertion that checks for equality with all smaller values.
assertLessThan :: String -> Int -> Int -> Assertion
assertLessThan s x y = assertBool s (x < y)

-- Assertion that checks for equality with all smaller or equal values.
assertLessThanEq :: String -> Int -> Int -> Assertion
assertLessThanEq s x y = assertBool s (x <= y)
