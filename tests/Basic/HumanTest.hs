module HumanTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit
import Test.QuickCheck
import Data.Graph

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
    commandToAction :: Board -> [Player] -> String -> Int -> Maybe Action
-}

commandToActionTest1 :: Test 
commandToActionTest1 = let 
    p = currentPlayer startingPlayers
    move = Just (Move (stepLeft (currentCell p))) 
    command = [columnLeft middleColumn] ++ "1" in
    TestCase (assertEqual 
        "commandToAction startingBoard startingPlayers command 0" 
        (commandToAction startingBoard startingPlayers command 0) move)

commandToActionTest2 :: Test 
commandToActionTest2 = let 
    p = currentPlayer startingPlayers
    move = Just (Move (stepRight (currentCell p))) 
    command = [columnRight middleColumn] ++ "1"  in
    TestCase (assertEqual 
        "commandToAction startingBoard startingPlayers command 0" 
        (commandToAction startingBoard startingPlayers command 0) move)

commandToActionTest3 :: Test 
commandToActionTest3 = let 
    p = currentPlayer startingPlayers
    wall = Just (Place (wallTop (currentCell p)))
    command = [middleColumn] ++ "1h" in
    TestCase (assertEqual 
        "commandToAction startingBoard startingPlayers command 0" 
        (commandToAction startingBoard startingPlayers command 0) wall)

commandToActionTest4 :: Test 
commandToActionTest4 = let 
    p = currentPlayer startingPlayers
    wall = Just (Place (wallRight (currentCell p))) 
    command = [middleColumn] ++ "1v" in
    TestCase (assertEqual 
        "commandToAction startingBoard startingPlayers command 0" 
        (commandToAction startingBoard startingPlayers command 0) wall)

-- All unit tests together.
humanPlayerUnitTests :: Spec
humanPlayerUnitTests = fromHUnitTest $
    TestList [
        TestLabel "commandToActionTest1" commandToActionTest1,
        TestLabel "commandToActionTest2" commandToActionTest2,
        TestLabel "commandToActionTest3" commandToActionTest3,
        TestLabel "commandToActionTest4" commandToActionTest4]
