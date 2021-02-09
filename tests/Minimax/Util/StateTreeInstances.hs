{-
    Module: StateTreeInstances.

    Used by quickcheck in the tests.
-}
module Util.StateTreeInstances where 

import Test.QuickCheck
import Control.Monad

import Types 

-- Generate an arbitrary state tree.
arbitraryTree :: (Arbitrary v, Arbitrary a) => Int -> Gen (StateTree v a) 
arbitraryTree 0 = do 
    x <- arbitrary 
    return (StateTree x [])
arbitraryTree n = do 
    m <- arbitrary 
    let n' = n `div` (m + 1)
    x <- arbitrary
    as <- replicateM m arbitrary
    ts <- replicateM m (arbitraryTree n')
    return (StateTree x (zip as ts))

-- Tell Haskell that we can generate arbitrary state trees.
instance (Arbitrary v, Arbitrary a) => Arbitrary (StateTree v a) where
    arbitrary = sized arbitraryTree

-- Describes how state trees are printed.
instance (Show v, Show a) => Show (StateTree v a) where 
    show (StateTree v ts) = "(StateTree " ++ (show v) ++ " " ++ (show ts) ++ ")"
