module GameTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Data.Graph

import Types
import Constants
import Cell 
import Action
import Game
import Players.Human 

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

-- Starting players in the usual positions. Note that the player type is irrelevant for this test.
startingPlayers :: [Player]
startingPlayers = 
    [makeHumanPlayer "X" (middleColumn, firstRow) wallsPerPlayer winningX, 
     makeHumanPlayer "Y" (middleColumn, lastRow) wallsPerPlayer winningY]
    where 
        middleColumn = intToColumn ((div boardSize 2) + 1)
        winningX = [(i, lastRow) | i<-allColumns]
        winningY = [(i, firstRow) | i<-allColumns]

-- Default game.
startingGame :: Game 
startingGame = Game startingBoard startingPlayers

{-
    Unit tests.
-}

{-
    currentPlayer :: [Player] -> Player 
-}

currentPlayerTest :: Test 
currentPlayerTest = 
    TestCase (assertEqual 
        "name (currentPlayer startingPlayers)" 
        (name (currentPlayer startingPlayers)) "X") 

{-
    previousPlayer :: [Player] -> Player 
-}

previousPlayerTest :: Test 
previousPlayerTest = 
    TestCase (assertEqual 
        "name (previousPlayer startingPlayers)" 
        (name (previousPlayer startingPlayers)) "Y") 

{-
    rotatePlayers :: [Player] -> [Player]
-}

rotatePlayersTest :: Test 
rotatePlayersTest = 
    TestCase (assertEqual 
        "name (currentPlayer (rotatePlayers startingPlayers))" 
        (name (currentPlayer (rotatePlayers startingPlayers))) "Y") 

{-
    validStepAction :: Game -> Player -> Step -> Bool
-}

validStepActionTestTrue :: Test
validStepActionTestTrue = let 
    p = currentPlayer startingPlayers 
    step = stepLeft (currentCell p) in
    TestCase (assertBool 
        "validStepAction startingGame step" 
        (validStepAction startingGame step)) 

validStepActionTestFalse :: Test
validStepActionTestFalse = let 
    p = currentPlayer startingPlayers 
    c = currentCell p
    step = (c, cellTop (cellTop c)) in
    TestCase (assertBool 
        "not (validStepAction startingGame step)" 
        (not (validStepAction startingGame step))) 

{-
    validWallAction :: Game -> Player -> Wall -> Bool
-}

validWallActionTestTrue :: Test
validWallActionTestTrue = let 
    p = currentPlayer startingPlayers 
    c = currentCell p 
    step1 = stepLeft c
    step2 = stepLeft (cellTop c)
    wall = (step1, step2) in
    TestCase (assertBool 
        "validWallAction startingGame wall" 
        (validWallAction startingGame wall)) 

validWallActionTestFalse :: Test
validWallActionTestFalse = let 
    p = currentPlayer startingPlayers 
    c = currentCell p 
    step1 = stepLeft c
    step2 = stepRight (cellTop c)
    wall = (step1, step2) in
    TestCase (assertBool 
        "not (validWallAction startingGame wall)" 
        (not (validWallAction startingGame wall))) 

{-
    performAction :: Game -> Action -> Maybe Game
-}

performActionTest1 :: Test
performActionTest1 = let 
    p = currentPlayer startingPlayers 
    c = currentCell p
    step = stepLeft c in
    case (performAction startingGame (Move step)) of 
        (Just (Game _ ps)) -> let 
            p' = previousPlayer ps 
            c' = currentCell p' in 
            TestCase (assertEqual "'performAction' move left" c' (cellLeft c))
        Nothing -> TestCase (assertBool "'performAction' move left fail" False)

performActionTest2 :: Test
performActionTest2 = let 
    p = currentPlayer startingPlayers 
    c = currentCell p
    step = (c, cellTop (cellTop c)) in
    case (performAction startingGame (Move step)) of 
        (Just _) -> TestCase (assertBool "'performAction' invalid move fail" False)
        Nothing -> TestCase (assertBool "'performAction' invalid move" True)

performActionTest3 :: Test
performActionTest3 = let 
    p = currentPlayer startingPlayers 
    c = currentCell p 
    step1 = stepLeft c
    step2 = stepLeft (cellTop c)
    wall = (step1, step2) in
    case (performAction startingGame (Place wall)) of 
        (Just (Game b _)) -> TestCase (assertEqual 
            "'performAction' place wall number of edges"
            (length (edges startingBoard)) ((length (edges b)) + 4)) -- Edges go both ways. 
        Nothing -> TestCase (assertBool "'performAction' place wall fail" False)

-- All unit tests together.
gameUnitTests :: Spec 
gameUnitTests = fromHUnitTest $
    TestList [
        TestLabel "currentPlayerTest" currentPlayerTest,
        TestLabel "previousPlayerTest" previousPlayerTest,
        TestLabel "validStepActionTestTrue" validStepActionTestTrue,
        TestLabel "validStepActionTestFalse" validStepActionTestFalse,
        TestLabel "validWallActionTestTrue" validWallActionTestTrue,
        TestLabel "validWallActionTestFalse" validWallActionTestFalse,
        TestLabel "performActionTest1" performActionTest1,
        TestLabel "performActionTest2" performActionTest2,
        TestLabel "performActionTest3" performActionTest3]
