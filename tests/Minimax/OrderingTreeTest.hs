{-
    Module: OrderingTreeTest.

    Used to test Part I.b.
-}
module OrderingTreeTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Types
import Players.Minimax (lowFirst)
import Util.StateTreeInstances

type TestTree = StateTree Int Int

{-
    Util.
-}

-- Retrieve value stored at the node.
nodeValue :: TestTree -> Int 
nodeValue (StateTree x _) = x

-- First subtree (used to check the second level).
firstSubtree :: TestTree -> TestTree 
firstSubtree (StateTree _ []) = error "No branches"
firstSubtree (StateTree _ (t:_)) = snd t

-- Get value of top branch.
topBranchValue :: TestTree -> Int 
topBranchValue t = nodeValue (firstSubtree t)

-- Simple function to check if a list is ordered.
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

{-
    Unit tests.
-}

-- Simple tree with two levels.
testTree :: TestTree 
testTree = StateTree 0 [(3, StateTree 3 ts), (4, StateTree 4 ts), (0, StateTree 0 ts)]
    where 
        ts = [(5, StateTree 5 []), (7, StateTree 7 []), (1, StateTree 1 [])]

-- Apply lowFirst and check that first value of top branch is the lowest.
testLowFirst1 :: Test 
testLowFirst1 = TestCase (assertEqual "topBranchValue (lowFirst testTree)" v 0)
    where 
        v = topBranchValue (lowFirst testTree)

-- Apply lowFirst and check that the first value of the top branch in the level below is the 
-- lowest.
testLowFirst2 :: Test 
testLowFirst2 = TestCase (assertEqual "topBranchValue (firstSubtree (lowFirst testTree))" v 1)
    where 
        v = topBranchValue (firstSubtree (lowFirst testTree))

-- All unit tests together.
orderingTreeUnitTests :: Spec
orderingTreeUnitTests = fromHUnitTest $
    TestList [
        TestLabel "testLowFirst1" testLowFirst1,
        TestLabel "testLowFirst2" testLowFirst2]

{-
    QuickCheck tests.
-}

-- Check if top level is ordered from highest to lowest.
testLowFirstQuickCheck :: TestTree -> Expectation
testLowFirstQuickCheck t = case lowFirst t of 
    (StateTree _ ts) -> assertBool "isSorted ts'" (isSorted ts')
        where 
            ts' = map (nodeValue . snd) ts

-- All QuickCheck tests together.
orderingTreeQuickCheckTests :: Spec
orderingTreeQuickCheckTests = do
    prop "testLowFirstQuickCheck" testLowFirstQuickCheck
