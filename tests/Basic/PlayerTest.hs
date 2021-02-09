module PlayerTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Types
import Constants
import Cell
import Action
import Player
import Game
import Players.Human

{-
    Some defaults.
-}

-- Column in the middle of the board.
middleColumn :: Column
middleColumn = intToColumn ((div boardSize 2) + 1)

-- Starting players in the usual positions. Note that the player type is irrelevant for this test.
startingPlayers :: [Player]
startingPlayers = 
    [makeHumanPlayer "X" (middleColumn, firstRow) wallsPerPlayer winningX, 
     makeHumanPlayer "Y" (middleColumn, lastRow) wallsPerPlayer winningY]
    where 
        winningX = [(i, lastRow) | i<-allColumns]
        winningY = [(i, firstRow) | i<-allColumns]

{-
    Unit tests.
-}

{-
    playerInCell :: [Player] -> Cell -> Maybe Player 
-}

playerInCellTest1 :: Test 
playerInCellTest1 = let c = (middleColumn, firstRow) in 
    case (playerInCell startingPlayers c) of 
        (Just p) -> TestCase (assertEqual "name of playerInCell" (name p) "X")
        Nothing -> TestCase (assertBool "fail playerInCell" False)

playerInCellTest2 :: Test 
playerInCellTest2 = let c = (middleColumn, lastRow) in 
    case (playerInCell startingPlayers c) of 
        (Just p) -> TestCase (assertEqual "name of playerInCell" (name p) "Y")
        Nothing -> TestCase (assertBool "fail playerInCell" False)

playerInCellTest3 :: Test 
playerInCellTest3 = let c = ('a', 1) in 
    case (playerInCell startingPlayers c) of 
        (Just p) -> TestCase (assertBool "fail playerInCell" False)
        Nothing -> TestCase (assertBool "playerInCell" True)

{-
    cellFree :: [Player] -> Cell -> Bool 
-}

cellFreeTest1 :: Test 
cellFreeTest1 = let c = (middleColumn, firstRow) in 
    TestCase (assertBool "not (cellFree startingPlayers c)" (not (cellFree startingPlayers c)))

cellFreeTest2 :: Test 
cellFreeTest2 = let c = (middleColumn, lastRow) in 
    TestCase (assertBool "not (cellFree startingPlayers c)" (not (cellFree startingPlayers c)))

cellFreeTest3 :: Test 
cellFreeTest3 = let c = ('a', 1) in 
    TestCase (assertBool "cellFree startingPlayers c" (cellFree startingPlayers c))

{-
    adjacentCells :: Player -> [Cell]
-}

adjacentCellsTest1 :: Test 
adjacentCellsTest1 = let 
    p = currentPlayer startingPlayers
    c = cellLeft (currentCell p) in 
    TestCase (assertBool "c `elem` adjacentCells p" (c `elem` adjacentCells p)) 

adjacentCellsTest2 :: Test 
adjacentCellsTest2 = let 
    p = currentPlayer startingPlayers
    c = cellTop (currentCell p) in 
    TestCase (assertBool "c `elem` adjacentCells p" (c `elem` adjacentCells p)) 

adjacentCellsTest3 :: Test 
adjacentCellsTest3 = let 
    p = currentPlayer startingPlayers
    c = cellTop (cellTop (currentCell p)) in 
    TestCase (assertBool "not (c `elem` adjacentCells p)" (not (c `elem` adjacentCells p)))

{-
    hasWon :: Player -> Bool
-}

hasWonTestTrue :: Test 
hasWonTestTrue = let 
    p = currentPlayer startingPlayers
    winningCell = head (winningPositions p)
    p' = p { currentCell = winningCell } in
    TestCase (assertBool "hasWon p'" (hasWon p'))

hasWonTestFalse :: Test 
hasWonTestFalse = let p = currentPlayer startingPlayers in
    TestCase (assertBool "not (hasWon p)" (not (hasWon p)))

{-
    hasWallsLeft :: Player -> Bool 
-}

hasWallsLeftTestTrue :: Test 
hasWallsLeftTestTrue = let p = currentPlayer startingPlayers in
    TestCase (assertBool "hasWallsLeft p" (hasWallsLeft p))

hasWallsLeftTestFalse :: Test 
hasWallsLeftTestFalse = let 
    p = currentPlayer startingPlayers
    p' = p { remainingWalls = 0 } in
    TestCase (assertBool "not (hasWallsLeft p')" (not (hasWallsLeft p')))

{-
    canMove :: Player -> [Player] -> Step -> Bool 
-}

canMoveTestTrue :: Test 
canMoveTestTrue = let 
    p = currentPlayer startingPlayers
    step = stepTop (currentCell p) in
    TestCase (assertBool "canMove p startingPlayers step" (canMove p startingPlayers step)) 

canMoveTestFalse :: Test 
canMoveTestFalse = let 
    p2 = previousPlayer startingPlayers
    p1 = (currentPlayer startingPlayers) { currentCell = cellBottom (currentCell p2) }
    step = stepTop (currentCell p1) in
    TestCase (assertBool 
        "not (canMove p1 startingPlayers step)" 
        (not (canMove p1 startingPlayers step))) 

{-
    movePlayer :: Player -> Step -> Player 
-}

movePlayerTest1 :: Test 
movePlayerTest1 = let 
    p = currentPlayer startingPlayers
    c = currentCell p 
    step = stepLeft c in 
    TestCase (assertEqual 
        "currentCell (movePlayer p step)" 
        (currentCell (movePlayer p step)) (cellLeft c))

movePlayerTest2 :: Test 
movePlayerTest2 = let 
    p = currentPlayer startingPlayers
    c = currentCell p 
    step = stepTop c in 
    TestCase (assertEqual 
        "currentCell (movePlayer p step)" 
        (currentCell (movePlayer p step)) (cellTop c))

{-
    useWall :: Player -> Player 
-}

useWallTest :: Test 
useWallTest = let 
    p = currentPlayer startingPlayers 
    walls = remainingWalls p in 
    TestCase (assertEqual "remainingWalls (useWall p)" (remainingWalls (useWall p)) (walls - 1))

{-
    nextTurn :: Player -> Player 
-}

nextTurnTest :: Test 
nextTurnTest = let 
    p = currentPlayer startingPlayers 
    t = turn p in 
    TestCase (assertEqual "remainingWalls (nextTurn p)" (turn (nextTurn p)) (t + 1))


-- All unit tests together.
playerUnitTests :: Spec 
playerUnitTests = fromHUnitTest $
    TestList [
        TestLabel "playerInCellTest1" playerInCellTest1,
        TestLabel "playerInCellTest2" playerInCellTest2,
        TestLabel "playerInCellTest3" playerInCellTest3,
        TestLabel "cellFreeTest1" cellFreeTest1,
        TestLabel "cellFreeTest2" cellFreeTest2,
        TestLabel "cellFreeTest3" cellFreeTest3,
        TestLabel "adjacentCellsTest1" adjacentCellsTest1,
        TestLabel "adjacentCellsTest2" adjacentCellsTest2,
        TestLabel "adjacentCellsTest3" adjacentCellsTest3,
        TestLabel "hasWonTestTrue" hasWonTestTrue,
        TestLabel "hasWonTestFalse" hasWonTestFalse,
        TestLabel "hasWallsLeftTestTrue" hasWallsLeftTestTrue,
        TestLabel "hasWallsLeftTestFalse" hasWallsLeftTestFalse,
        TestLabel "canMoveTestTrue" canMoveTestTrue,
        TestLabel "canMoveTestFalse" canMoveTestFalse,
        TestLabel "movePlayerTest1" movePlayerTest1,
        TestLabel "movePlayerTest2" movePlayerTest2,
        TestLabel "useWallTest" useWallTest,
        TestLabel "nextTurnTest" nextTurnTest]
