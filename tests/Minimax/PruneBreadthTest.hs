{-
    Module: PruneBreadthTest.

    Used to test Part I.d.
-}
module PruneBreadthTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Types
import Players.Minimax
import Util.Assertion
import Util.StateTreeInstances

type TestTree = StateTree Int Int

{-
    Unit tests.
-}

-- Tree of breadth 0.
testTree0 :: TestTree
testTree0 = StateTree 6 [] 

-- Tree of breadth 1.
testTree1 :: TestTree 
testTree1 = StateTree 8 [(1, testTree0)]

-- Tree of breadth 2.
testTree2 :: TestTree 
testTree2 = StateTree 4 [(2, testTree1), (3, testTree0)]

-- Tree of breadth 3.
testTree3 :: TestTree 
testTree3 = StateTree 1 [(5, testTree0), (7, testTree2), (1, testTree1)]

-- Pruning at breadth 0.
testPruneBreadth0 :: Test 
testPruneBreadth0 = TestCase (assertEqual "pruneBreadth 0 testTree3" breadth 0)
    where 
        breadth = stateTreeBreadth (pruneBreadth 0 testTree3)

-- Pruning at breadth 1.
testPruneBreadth1 :: Test 
testPruneBreadth1 = TestCase (assertEqual "pruneBreadth 1 testTree3" breadth 1)
    where 
        breadth = stateTreeBreadth (pruneBreadth 1 testTree3)

-- Pruning at breadth 2.
testPruneBreadth2 :: Test 
testPruneBreadth2 = TestCase (assertEqual "pruneBreadth 2 testTree3" breadth 2)
    where 
        breadth = stateTreeBreadth (pruneBreadth 2 testTree3)

-- Pruning at breadth 3.
testPruneBreadth3 :: Test 
testPruneBreadth3 = TestCase (assertEqual "pruneBreadth 3 testTree3" breadth 3)
    where 
        breadth = stateTreeBreadth (pruneBreadth 3 testTree3)

-- All unit tests together.
pruneBreadthUnitTests :: Spec
pruneBreadthUnitTests = fromHUnitTest $
    TestList [
        TestLabel "testPruneBreadth0" testPruneBreadth0,
        TestLabel "testPruneBreadth1" testPruneBreadth1,
        TestLabel "testPruneBreadth2" testPruneBreadth2,
        TestLabel "testPruneBreadth3" testPruneBreadth3]

{-
    QuickCheck tests.
-}

-- Pruning at breadth 0 should always return trees of breadth 0.
testPruneBreadthQuickCheck0 :: TestTree -> Expectation
testPruneBreadthQuickCheck0 t = 
    assertEqual "stateTreeBreadth (pruneBreadth 0 t)" (stateTreeBreadth (pruneBreadth 0 t)) 0

-- Pruning at breadth 5 should always return trees of breadth less than 5.
testPruneBreadthQuickCheck5 :: TestTree -> Expectation
testPruneBreadthQuickCheck5 t = 
    assertLessThanEq "stateTreeBreadth (pruneBreadth 5 t)" (stateTreeBreadth (pruneBreadth 5 t)) 5

-- Pruning at breadth 10 should always return trees of breadth less than 10.
testPruneBreadthQuickCheck10 :: TestTree -> Expectation
testPruneBreadthQuickCheck10 t = 
    assertLessThanEq "stateTreeBreadth (pruneBreadth 10 t)" (stateTreeBreadth (pruneBreadth 10 t)) 10

-- All QuickCheck tests together.
pruneBreadthQuickCheckTests :: Spec
pruneBreadthQuickCheckTests = do
    prop "testPruneBreadthQuickCheck0" testPruneBreadthQuickCheck0
    prop "testPruneBreadthQuickCheck5" testPruneBreadthQuickCheck5
    prop "testPruneBreadthQuickCheck10" testPruneBreadthQuickCheck10
