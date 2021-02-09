{-
    Module: PruneDepthTest.

    Used to test Part I.c.
-}
module PruneDepthTest where 

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

-- Tree of depth 0.
testTree0 :: TestTree
testTree0 = StateTree 6 [] 

-- Tree of depth 1.
testTree1 :: TestTree 
testTree1 = StateTree 8 [(1, testTree0), (3, testTree0)]

-- Tree of depth 2.
testTree2 :: TestTree 
testTree2 = StateTree 4 [(2, testTree1)]

-- Tree of depth 3.
testTree3 :: TestTree 
testTree3 = StateTree 1 [(5, testTree2), (7, testTree2), (1, testTree2)]

-- Pruning at depth 0.
testPruneDepth0 :: Test 
testPruneDepth0 = TestCase (assertEqual "pruneDepth 0 testTree3" depth 0)
    where 
        depth = stateTreeDepth (pruneDepth 0 testTree3)

-- Pruning at depth 1.
testPruneDepth1 :: Test 
testPruneDepth1 = TestCase (assertEqual "pruneDepth 1 testTree3" depth 1)
    where 
        depth = stateTreeDepth (pruneDepth 1 testTree3)

-- Pruning at depth 2.
testPruneDepth2 :: Test 
testPruneDepth2 = TestCase (assertEqual "pruneDepth 2 testTree3" depth 2)
    where 
        depth = stateTreeDepth (pruneDepth 2 testTree3)

-- Pruning at depth 3.
testPruneDepth3 :: Test 
testPruneDepth3 = TestCase (assertEqual "pruneDepth 3 testTree3" depth 3)
    where 
        depth = stateTreeDepth (pruneDepth 3 testTree3)

-- All unit tests together.
pruneDepthUnitTests :: Spec
pruneDepthUnitTests = fromHUnitTest $
    TestList [
        TestLabel "testPruneDepth0" testPruneDepth0,
        TestLabel "testPruneDepth1" testPruneDepth1,
        TestLabel "testPruneDepth2" testPruneDepth2,
        TestLabel "testPruneDepth3" testPruneDepth3]

{-
    QuickCheck tests.
-}

-- Pruning at depth 0 should always return trees of depth 0.
testPruneDepthQuickCheck0 :: TestTree -> Expectation
testPruneDepthQuickCheck0 t = 
    assertEqual "stateTreeDepth (pruneDepth 0 t)" (stateTreeDepth (pruneDepth 0 t)) 0

-- Pruning at depth 5 should always return trees of depth less than 5.
testPruneDepthQuickCheck5 :: TestTree -> Expectation
testPruneDepthQuickCheck5 t = 
    assertLessThanEq "stateTreeDepth (pruneDepth 5 t)" (stateTreeDepth (pruneDepth 5 t)) 5

-- Pruning at depth 10 should always return trees of depth less than 10.
testPruneDepthQuickCheck10 :: TestTree -> Expectation
testPruneDepthQuickCheck10 t = 
    assertLessThanEq "stateTreeDepth (pruneDepth 10 t)" (stateTreeDepth (pruneDepth 10 t)) 10

-- All QuickCheck tests together.
pruneDepthQuickCheckTests :: Spec
pruneDepthQuickCheckTests = do
    prop "testPruneDepthQuickCheck0" testPruneDepthQuickCheck0
    prop "testPruneDepthQuickCheck5" testPruneDepthQuickCheck5
    prop "testPruneDepthQuickCheck10" testPruneDepthQuickCheck10
