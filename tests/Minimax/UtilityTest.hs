{-
    Module: UtilityTest.

    Used to test Part I.e.
-}
module UtilityTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Data.Maybe
import Data.Graph

import Types
import Constants
import Cell
import Action
import Game 
import Players.Minimax
import Util.Assertion

{-
    Generating the default starting game.
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
    [makeMinimaxPlayer "X" (middleColumn, firstRow) wallsPerPlayer winningX, 
     makeMinimaxPlayer "Y" (middleColumn, lastRow) wallsPerPlayer winningY]
    where 
        middleColumn = intToColumn ((div boardSize 2) + 1)
        winningX = [(i, lastRow) | i<-allColumns]
        winningY = [(i, firstRow) | i<-allColumns]

-- Starting board and starting players together.
startingGame :: Game 
startingGame = Game startingBoard startingPlayers

{-
    Unit tests.

    For this part there are only two unit tests that take into account how moving and wall placement
    may affect the value of the utility function.
-}

-- Game where X moves one position ahead and Y one position to the left.
positionAheadGame :: Game 
positionAheadGame = fromJust $ performAction (fromJust $ performAction startingGame actX) actY
    where 
        actX, actY :: Action
        actX = let c = currentCell (currentPlayer startingPlayers) in Move (stepTop c)
        actY = let c = currentCell (previousPlayer startingPlayers) in Move (stepLeft c)

-- The utility function should give a higher value to 'positionAheadGame'.
testUtilityPositionAhead :: Test 
testUtilityPositionAhead = 
    TestCase (assertLessThan "positionAheadGame" (utility startingGame) (utility positionAheadGame))

-- Game where X moves one position to the right and Y places a wall in front of it.
wallInFrontGame :: Game
wallInFrontGame = fromJust $ performAction (fromJust $ performAction startingGame actX) actY
    where 
        actX, actY :: Action
        actX = let c = currentCell (currentPlayer startingPlayers) in Move (stepRight c)
        actY = let c = currentCell (currentPlayer startingPlayers) in Place (wallTop c)

-- The utility function should take into account walls and hence give a lower value to 
-- 'wallInFrontGame'.
testUtilityWallInFront :: Test
testUtilityWallInFront =
    TestCase (assertLessThan "wallInFrontGame" (utility wallInFrontGame) (utility startingGame))

-- All unit tests together.
utilityUnitTests :: Spec
utilityUnitTests = fromHUnitTest $
    TestList [
        TestLabel "testUtilityPositionAhead" testUtilityPositionAhead,
        TestLabel "testUtilityWallInFront" testUtilityWallInFront]
