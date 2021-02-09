module CellTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Types
import Constants
import Cell

{-
    Unit tests.
-}

{-
    columnToInt :: Column -> Int 
-}

columnToIntTest1 :: Test 
columnToIntTest1 = TestCase (assertEqual "columnToInt 'a'" (columnToInt 'a') 1)

columnToIntTest2 :: Test 
columnToIntTest2 = TestCase (assertEqual "columnToInt 'e'" (columnToInt 'e') 5)

{-
   intToColumn :: Int -> Column 
-}

intToColumnTest1 :: Test 
intToColumnTest1 = TestCase (assertEqual "intToColumn 1" (intToColumn 1) 'a')

intToColumnTest2 :: Test 
intToColumnTest2 = TestCase (assertEqual "intToColumn 5" (intToColumn 5) 'e')

{-
    columnLeft :: Column -> Column 
-}

columnLeftTest1 :: Test 
columnLeftTest1 = TestCase (assertEqual "columnLeft 'b'" (columnLeft 'b') 'a')

columnLeftTest2 :: Test 
columnLeftTest2 = TestCase (assertEqual "columnLeft 'e'" (columnLeft 'e') 'd')

{-
    columnRight :: Column -> Column 
-}

columnRightTest1 :: Test 
columnRightTest1 = TestCase (assertEqual "columnRight 'a'" (columnRight 'a') 'b')

columnRightTest2 :: Test 
columnRightTest2 = TestCase (assertEqual "columnRight 'd'" (columnRight 'd') 'e')

{-
    cellLeft :: Cell -> Cell 
-}

cellLeftTest1 :: Test 
cellLeftTest1 = TestCase (assertEqual "cellLeft ('b', 1)" (cellLeft ('b', 1)) ('a', 1))

cellLeftTest2 :: Test 
cellLeftTest2 = TestCase (assertEqual "cellLeft ('e', 2)" (cellLeft ('e', 2)) ('d', 2))

{-
    cellRight :: Cell -> Cell 
-}

cellRightTest1 :: Test 
cellRightTest1 = TestCase (assertEqual "cellRight ('a', 1)" (cellRight ('a', 1)) ('b', 1))

cellRightTest2 :: Test 
cellRightTest2 = TestCase (assertEqual "cellRight ('d', 2)" (cellRight ('d', 2)) ('e', 2))

{-
    cellTop :: Cell -> Cell 
-}

cellTopTest1 :: Test 
cellTopTest1 = TestCase (assertEqual "cellTop ('a', 1)" (cellTop ('a', 1)) ('a', 2))

cellTopTest2 :: Test 
cellTopTest2 = TestCase (assertEqual "cellTop ('d', 2)" (cellTop ('d', 2)) ('d', 3))

{-
    cellBottom :: Cell -> Cell 
-}

cellBottomTest1 :: Test 
cellBottomTest1 = TestCase (assertEqual "cellBottom ('a', 2)" (cellBottom ('a', 2)) ('a', 1))

cellBottomTest2 :: Test 
cellBottomTest2 = TestCase (assertEqual "cellBottom ('d', 3)" (cellBottom ('d', 3)) ('d', 2))

{-
    cellInBoard :: Cell -> Bool 
-}

cellInBoardTestTrue1 :: Test 
cellInBoardTestTrue1 = TestCase (assertBool "cellInBoard ('a', 1)" (cellInBoard ('a', 1)))

cellInBoardTestTrue2 :: Test 
cellInBoardTestTrue2 = let lastCell = (intToColumn boardSize, boardSize) in
    TestCase (assertBool "cellInBoard lastCell" (cellInBoard lastCell))

cellInBoardTestFalse1 :: Test 
cellInBoardTestFalse1 = 
    TestCase (assertBool "not (cellInBoard ('a', 0))" (not (cellInBoard ('a', 0))))

cellInBoardTestFalse2 :: Test 
cellInBoardTestFalse2 = let outCell = (intToColumn (boardSize + 1), boardSize) in
    TestCase (assertBool "not (cellInBoard outCell)" (not (cellInBoard outCell)))

{-
    distance :: Cell -> Cell -> Int 
-}

distanceTest1 :: Test 
distanceTest1 = TestCase (assertEqual "distance ('a', 1) ('a', 2)" (distance ('a', 1) ('a', 2)) 1)

distanceTest2 :: Test 
distanceTest2 = TestCase (assertEqual "distance ('a', 1) ('b', 1)" (distance ('a', 1) ('b', 1)) 1)

distanceTest3 :: Test 
distanceTest3 = TestCase (assertEqual "distance ('c', 2) ('d', 4)" (distance ('c', 2) ('d', 4)) 3)

{-
    isAdjacent :: Cell -> Cell -> Bool 
-}

isAdjacentTest1 :: Test 
isAdjacentTest1 = 
    TestCase (assertBool "isAdjacent ('a', 1) ('a', 2)" (isAdjacent ('a', 1) ('a', 2)))

isAdjacentTest2 :: Test 
isAdjacentTest2 = 
    TestCase (assertBool "isAdjacent ('a', 1) ('b', 1)" (isAdjacent ('a', 1) ('b', 1)))

isAdjacentTest3 :: Test 
isAdjacentTest3 = 
    TestCase (assertBool "not (isAdjacent ('c', 2) ('d', 4))" (not (isAdjacent ('c', 2) ('d', 4))))

{-
    isHorizontallyAdjacent :: Cell -> Cell -> Bool 
-}

isHorizontallyAdjacentTest1 :: Test 
isHorizontallyAdjacentTest1 = 
    TestCase (assertBool 
        "not (isHorizontallyAdjacent ('a', 1) ('a', 2))" 
        (not (isHorizontallyAdjacent ('a', 1) ('a', 2))))

isHorizontallyAdjacentTest2 :: Test 
isHorizontallyAdjacentTest2 = 
    TestCase (assertBool 
        "isHorizontallyAdjacent ('a', 1) ('b', 1)" 
        (isHorizontallyAdjacent ('a', 1) ('b', 1)))

isHorizontallyAdjacentTest3 :: Test 
isHorizontallyAdjacentTest3 = 
    TestCase (assertBool 
        "not (isHorizontallyAdjacent ('c', 2) ('d', 4))" 
        (not (isHorizontallyAdjacent ('c', 2) ('d', 4))))

{-
    isVerticallyAdjacent :: Cell -> Cell -> Bool
-}

isVerticallyAdjacentTest1 :: Test 
isVerticallyAdjacentTest1 = 
    TestCase (assertBool 
        "isVerticallyAdjacent ('a', 1) ('a', 2)" 
        (isVerticallyAdjacent ('a', 1) ('a', 2)))

isVerticallyAdjacentTest2 :: Test 
isVerticallyAdjacentTest2 = 
    TestCase (assertBool 
        "not (isVerticallyAdjacent ('a', 1) ('b', 1))" 
        (not (isVerticallyAdjacent ('a', 1) ('b', 1))))

isVerticallyAdjacentTest3 :: Test 
isVerticallyAdjacentTest3 = 
    TestCase (assertBool 
        "not (isVerticallyAdjacent ('c', 2) ('d', 4))" 
        (not (isVerticallyAdjacent ('c', 2) ('d', 4))))

-- All unit tests together.
cellUnitTests :: Spec
cellUnitTests = fromHUnitTest $
    TestList [
        TestLabel "columnToIntTest1" columnToIntTest1,
        TestLabel "columnToIntTest2" columnToIntTest2,
        TestLabel "intToColumnTest1" intToColumnTest1,
        TestLabel "intToColumnTest2" intToColumnTest2,
        TestLabel "cellLeftTest1" cellLeftTest1,
        TestLabel "cellLeftTest2" cellLeftTest2,
        TestLabel "cellRightTest1" cellRightTest1,
        TestLabel "cellRightTest2" cellRightTest2,
        TestLabel "cellTopTest1" cellTopTest1,
        TestLabel "cellTopTest2" cellTopTest2,
        TestLabel "cellInBoardTestTrue1" cellInBoardTestTrue1,
        TestLabel "cellInBoardTestTrue2" cellInBoardTestTrue2,
        TestLabel "cellInBoardTestFalse1" cellInBoardTestFalse1,
        TestLabel "cellInBoardTestFalse2" cellInBoardTestFalse2,
        TestLabel "distanceTest1" distanceTest1,
        TestLabel "distanceTest2" distanceTest2,
        TestLabel "distanceTest3" distanceTest3,
        TestLabel "isAdjacentTest1" isAdjacentTest1,
        TestLabel "isAdjacentTest2" isAdjacentTest2,
        TestLabel "isAdjacentTest3" isAdjacentTest3,
        TestLabel "isHorizontallyAdjacentTest1" isHorizontallyAdjacentTest1,
        TestLabel "isHorizontallyAdjacentTest2" isHorizontallyAdjacentTest2,
        TestLabel "isHorizontallyAdjacentTest3" isHorizontallyAdjacentTest3,
        TestLabel "isVerticallyAdjacentTest1" isVerticallyAdjacentTest1,
        TestLabel "isVerticallyAdjacentTest2" isVerticallyAdjacentTest2,
        TestLabel "isVerticallyAdjacentTest3" isVerticallyAdjacentTest3]

{-
    QuickCheck tests.
-}

{-
    cellToIndex :: Cell -> Index 
    indexToCell :: Index -> Cell 
-}

cellToIndexIndexToCell :: Int -> Expectation
cellToIndexIndexToCell i = assertEqual "cellToIndex (indexToCell i)" (cellToIndex (indexToCell i)) i

indexToCellCellToIndex :: Cell -> Expectation
indexToCellCellToIndex c = assertEqual "indexToCell (cellToIndex c)" (indexToCell (cellToIndex c)) c

-- All QuickCheck tests together.
cellQuickCheckTests :: Spec
cellQuickCheckTests = do
    prop "cellToIndexIndexToCell" cellToIndexIndexToCell
    prop "cellToIndexIndexToCell" cellToIndexIndexToCell
