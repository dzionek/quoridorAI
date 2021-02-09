module BoardTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Data.Graph

import Types
import Constants
import Cell
import Board

{-
    Some defaults.
-}

-- All the cells.
startingCells :: [Cell]
startingCells = [(i, j) | i<-allColumns, j<-allRows]

-- All the edges.
startingEdges :: [(Cell, Cell, [Cell])]
startingEdges = [(c, c, adjacent c) | c<-startingCells]
    where 
        adjacent :: Cell -> [Cell]
        adjacent c = [c' | c'<-startingCells, isAdjacent c c']

-- A board (graph) formed from all the edges.
startingBoard :: Board 
startingBoard = b 
    where 
        (b, _, _) = graphFromEdges startingEdges

{-
    Unit tests.
-}

{-
    validStep :: Board -> Step -> Bool 
-}

validStepTestTrue1 :: Test 
validStepTestTrue1 = let step = (('a', 1), ('b', 1)) in
    TestCase (assertBool "validStep startingBoard step" (validStep startingBoard step))

validStepTestTrue2 :: Test 
validStepTestTrue2 = let step = (('a', 1), ('a', 2)) in
    TestCase (assertBool "validStep startingBoard step" (validStep startingBoard step))

validStepTestFalse1 :: Test 
validStepTestFalse1 = let step = (('a', 1), ('c', 1)) in
    TestCase (assertBool "not (validStep startingBoard step)" (not (validStep startingBoard step)))

validStepTestFalse2 :: Test 
validStepTestFalse2 = let step = (('a', 1), ('a', 3)) in
    TestCase (assertBool "not (validStep startingBoard step)" (not (validStep startingBoard step)))

{-
    reachableCells :: Board -> Cell -> [Cell]
-}

reachableCellsTest1 :: Test 
reachableCellsTest1 = let 
    c1 = ('a', 1)
    c2 = ('a', 2) in 
    TestCase (assertBool 
        "c2 `elem` (reachableCells startingBoard c1)" 
        (c2 `elem` (reachableCells startingBoard c1)))

reachableCellsTest2 :: Test 
reachableCellsTest2 = let 
    c1 = ('a', 1)
    c2 = ('b', 1) in 
    TestCase (assertBool 
        "c2 `elem` (reachableCells startingBoard c1)" 
        (c2 `elem` (reachableCells startingBoard c1)))

reachableCellsTest3 :: Test 
reachableCellsTest3 = let 
    c1 = ('a', 1)
    c2 = ('c', 1) in 
    TestCase (assertBool 
        "not (c2 `elem` (reachableCells startingBoard c1))" 
        (not (c2 `elem` (reachableCells startingBoard c1))))

reachableCellsTest4 :: Test 
reachableCellsTest4 = let 
    c1 = ('a', 1)
    c2 = ('a', 2) in 
    TestCase (assertBool 
        "not (c2 `elem` (reachableCells (removeStep startingBoard (c1, c2)) c1))" 
        (not (c2 `elem` (reachableCells (removeStep startingBoard (c1, c2)) c1))))

{-
    parallelSteps :: Board -> Step -> Step -> Bool 
-}

parallelStepsTrue1 :: Test 
parallelStepsTrue1 = let 
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 2), ('b', 2)) in 
    TestCase (assertBool 
        "parallelSteps startingBoard step1 step2" 
        (parallelSteps startingBoard step1 step2))

parallelStepsTrue2 :: Test 
parallelStepsTrue2 = let 
    step1 = (('a', 1), ('a', 2))
    step2 = (('b', 1), ('b', 2)) in 
    TestCase (assertBool 
        "parallelSteps startingBoard step1 step2" 
        (parallelSteps startingBoard step1 step2))

parallelStepsFalse1 :: Test 
parallelStepsFalse1 = let 
    step1 = (('b', 1), ('c', 1))
    step2 = (('b', 1), ('a', 1)) in 
    TestCase (assertBool 
        "not (parallelSteps startingBoard step1 step2)" 
        (not (parallelSteps startingBoard step1 step2)))

parallelStepsFalse2 :: Test 
parallelStepsFalse2 = let 
    step1 = (('a', 1), ('a', 2))
    step2 = (('a', 2), ('a', 3)) in 
    TestCase (assertBool 
        "not (parallelSteps startingBoard step1 step2)" 
        (not (parallelSteps startingBoard step1 step2)))

{-
    noCrossingWalls :: Board -> Step -> Step -> Bool
-}

noCrossingWallsTrue :: Test 
noCrossingWallsTrue = let 
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 2), ('b', 2)) in 
    TestCase (assertBool 
        "noCrossingWalls startingBoard step1 step2" 
        (noCrossingWalls startingBoard step1 step2))

noCrossingWallsFalse :: Test 
noCrossingWallsFalse = let 
    stepRight1 = (('a', 1), ('b', 1))
    stepRight2 = (('a', 2), ('b', 2)) 
    stepTop1 = (('a', 1), ('a', 2))
    stepTop2 = (('b', 1), ('b', 2))
    wall = (stepRight1, stepRight2) in 
    TestCase (assertBool 
        "not (noCrossingWalls (placeWall startingBoard wall) stepTop1 stepTop2)" 
        (not (noCrossingWalls (placeWall startingBoard wall) stepTop1 stepTop2)))

{-
    validWallSteps :: Board -> Wall -> Bool 
-}

validWallStepsTrue :: Test 
validWallStepsTrue = let 
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 2), ('b', 2)) 
    wall = (step1, step2) in 
    TestCase (assertBool 
        "validWallSteps startingBoard wall" 
        (validWallSteps startingBoard wall))

validWallStepsFalse :: Test 
validWallStepsFalse = let 
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 1), ('a', 3)) 
    wall = (step1, step2) in 
    TestCase (assertBool 
        "not (validWallSteps startingBoard wall)" 
        (not (validWallSteps startingBoard wall)))

{-
    validWall :: Board -> Wall -> Bool 
-}

validWallTrue :: Test 
validWallTrue = let 
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 2), ('b', 2)) 
    wall = (step1, step2) in 
    TestCase (assertBool 
        "validWall startingBoard wall" 
        (validWall startingBoard wall))

validWallFalse1 :: Test 
validWallFalse1 = let 
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 1), ('a', 3)) 
    wall = (step1, step2) in 
    TestCase (assertBool 
        "not (validWall startingBoard wall)" 
        (not (validWall startingBoard wall)))

validWallFalse2 :: Test 
validWallFalse2 = let 
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 1), ('a', 2)) 
    wall = (step1, step2) in 
    TestCase (assertBool 
        "not (validWall startingBoard wall)" 
        (not (validWall startingBoard wall)))

validWallFalse3 :: Test 
validWallFalse3 = let 
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 2), ('b', 2)) 
    wall = (step1, step2) in 
    TestCase (assertBool 
        "not (validWall (placeWall startingBoard wall) wall)" 
        (not (validWall (placeWall startingBoard wall) wall)))

validWallFalse4 :: Test 
validWallFalse4 = let 
    stepRight1 = (('a', 1), ('b', 1))
    stepRight2 = (('a', 2), ('b', 2)) 
    stepTop1 = (('a', 1), ('a', 2))
    stepTop2 = (('b', 1), ('b', 2))
    wallRight = (stepRight1, stepRight2) 
    wallTop = (stepTop1, stepTop2) in 
    TestCase (assertBool 
        "not (validWall (placeWall startingBoard wallRight) wallTop)" 
        (not (validWall (placeWall startingBoard wallRight) wallTop)))

{-
    removeStep :: Board -> Step -> Board 
-}

removeStepTest1 :: Test 
removeStepTest1 = let step = (('a', 1), ('b', 1)) in 
    TestCase (assertBool 
        "not (validStep (removeStep startingBoard step) step)" 
        (not (validStep (removeStep startingBoard step) step)))

removeStepTest2 :: Test 
removeStepTest2 = let 
    step1 = (('a', 1), ('b', 1)) 
    step2 = (('a', 1), ('a', 2)) in 
    TestCase (assertBool 
        "validStep (removeStep startingBoard step1) step2" 
        (validStep (removeStep startingBoard step1) step2))

{-
    placeWall :: Board -> Wall -> Board 
-}

placeWallTest1 :: Test 
placeWallTest1 = let
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 2), ('b', 2)) 
    wall = (step1, step2) in 
    TestCase (assertBool 
        "not (validStep (placeWall startingBoard wall) step1)" 
        (not (validStep (placeWall startingBoard wall) step1)))

placeWallTest2 :: Test 
placeWallTest2 = let
    step1 = (('a', 1), ('b', 1))
    step2 = (('a', 2), ('b', 2)) 
    step3 = (('a', 1), ('a', 2))
    wall = (step1, step2) in 
    TestCase (assertBool 
        "validStep (placeWall startingBoard wall) step3" 
        (validStep (placeWall startingBoard wall) step3))

-- All unit tests together.
boardUnitTests :: Spec
boardUnitTests = fromHUnitTest $
    TestList [
        TestLabel "validStepTestTrue1" validStepTestTrue1,
        TestLabel "validStepTestTrue2" validStepTestTrue2,
        TestLabel "validStepTestFalse1" validStepTestFalse1,
        TestLabel "validStepTestFalse2" validStepTestFalse2,
        TestLabel "reachableCellsTest1" reachableCellsTest1,
        TestLabel "reachableCellsTest2" reachableCellsTest2,
        TestLabel "reachableCellsTest3" reachableCellsTest3,
        TestLabel "reachableCellsTest4" reachableCellsTest4,
        TestLabel "parallelStepsTrue1" parallelStepsTrue1,
        TestLabel "parallelStepsTrue2" parallelStepsTrue2,
        TestLabel "parallelStepsFalse1" parallelStepsFalse1,
        TestLabel "parallelStepsFalse2" parallelStepsFalse2,
        TestLabel "noCrossingWallsTrue" noCrossingWallsTrue,
        TestLabel "noCrossingWallsFalse" noCrossingWallsFalse,
        TestLabel "validWallStepsTrue" validWallStepsTrue,
        TestLabel "validWallStepsFalse" validWallStepsFalse,
        TestLabel "validWallTrue" validWallTrue,
        TestLabel "validWallFalse1" validWallFalse1,
        TestLabel "validWallFalse2" validWallFalse2,
        TestLabel "validWallFalse3" validWallFalse3,
        TestLabel "validWallFalse4" validWallFalse4,
        TestLabel "removeStepTest1" removeStepTest1,
        TestLabel "removeStepTest2" removeStepTest2,
        TestLabel "placeWallTest1" placeWallTest1,
        TestLabel "placeWallTest2" placeWallTest2]
