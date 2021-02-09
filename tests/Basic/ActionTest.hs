module ActionTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit
import Test.QuickCheck

import Action

{-
    Unit tests.
-}

{-
    stepLeft :: Cell -> Step 
-}

stepLeftTest1 :: Test 
stepLeftTest1 = 
    TestCase (assertEqual "stepLeft ('b', 1)" (stepLeft ('b', 1)) (('b', 1), ('a', 1)))

stepLeftTest2 :: Test 
stepLeftTest2 = 
    TestCase (assertEqual "stepLeft ('e', 2)" (stepLeft ('e', 2)) (('e', 2), ('d', 2)))

{-
    stepRight :: Cell -> Step 
-}

stepRightTest1 :: Test 
stepRightTest1 = 
    TestCase (assertEqual "stepRight ('a', 1)" (stepRight ('a', 1)) (('a', 1), ('b', 1)))

stepRightTest2 :: Test 
stepRightTest2 = 
    TestCase (assertEqual "stepRight ('d', 2)" (stepRight ('d', 2)) (('d', 2), ('e', 2)))

{-
    stepTop :: Cell -> Step 
-}

stepTopTest1 :: Test 
stepTopTest1 = 
    TestCase (assertEqual "stepTop ('a', 1)" (stepTop ('a', 1)) (('a', 1), ('a', 2)))

stepTopTest2 :: Test 
stepTopTest2 = 
    TestCase (assertEqual "stepTop ('d', 2)" (stepTop ('d', 2)) (('d', 2), ('d', 3)))

{-
    stepBottom :: Cell -> Step 
-}

stepBottomTest1 :: Test 
stepBottomTest1 = 
    TestCase (assertEqual "stepBottom ('a', 2)" (stepBottom ('a', 2)) (('a', 2), ('a', 1)))

stepBottomTest2 :: Test 
stepBottomTest2 = 
    TestCase (assertEqual "stepBottom ('d', 3)" (stepBottom ('d', 3)) (('d', 3), ('d', 2)))

{-
    wallLeft :: Cell -> Wall 
-}

wallLeftTest :: Test 
wallLeftTest = let 
    step1 = (('b', 2), ('a', 2))
    step2 = (('b', 1), ('a', 1)) in
    TestCase (assertEqual "wallLeft ('b', 2)" (wallLeft ('b', 2)) (step1, step2))

{-
    wallRight :: Cell -> Wall 
-}

wallRightTest :: Test 
wallRightTest = let 
    step1 = (('b', 2), ('c', 2))
    step2 = (('b', 3),( 'c', 3)) in
    TestCase (assertEqual "wallRight ('b', 2)" (wallRight ('b', 2)) (step1, step2))

{-
    wallTop :: Cell -> Wall 
-}

wallTopTest :: Test 
wallTopTest = let 
    step1 = (('b', 2), ('b', 3))
    step2 = (('c', 2), ('c', 3)) in
    TestCase (assertEqual "wallTop ('b', 2)" (wallTop ('b', 2)) (step1, step2))

{-
    wallBottom :: Cell -> Wall 
-}

wallBottomTest :: Test 
wallBottomTest = let 
    step1 = (('b', 2), ('b', 1))
    step2 = (('a', 2), ('a', 1)) in
    TestCase (assertEqual "wallBottom ('b', 2)" (wallBottom ('b', 2)) (step1, step2))

-- All unit tests together.
actionUnitTests :: Spec
actionUnitTests = fromHUnitTest $
    TestList [
        TestLabel "stepLeftTest1" stepLeftTest1,
        TestLabel "stepLeftTest2" stepLeftTest2,
        TestLabel "stepRightTest1" stepRightTest1,
        TestLabel "stepRightTest2" stepRightTest2,
        TestLabel "stepTopTest1" stepTopTest1,
        TestLabel "stepTopTest2" stepTopTest2,
        TestLabel "stepBottomTest1" stepBottomTest1,
        TestLabel "stepBottomTest2" stepBottomTest2,
        TestLabel "wallLeftTest" wallLeftTest,
        TestLabel "wallRightTest" wallRightTest,
        TestLabel "wallTopTest" wallTopTest,
        TestLabel "wallBottomTest" wallBottomTest]
