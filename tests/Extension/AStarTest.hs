{-
    Module: AStarTest.

    Used as an extension to test the AStar module.
-}
module AStarTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit ( assertEqual, Test(..) )
import Data.Maybe ( fromJust )
import Data.Graph ( graphFromEdges )


import AStar (aStar)
import Board (placeWall)
import Action ( wallTop, wallRight )
import Types ( Board, Cell )
import Cell ( isAdjacent )
import Constants ( allRows, allColumns )

--------------------------------------------
-- Taken from Main.hs in the main package --
--------------------------------------------
-- It was the best choice for me to just copy paste the relevant code even though its duplicated.
-- The problem is that Main.hs should be both the entry point for tests and the actual program.
startingCells :: [Cell]
startingCells = [(i, j) | i<-allColumns, j<-allRows]

startingEdges :: [(Cell, Cell, [Cell])]
startingEdges = [(c, c, adjacent c) | c<-startingCells]
    where 
        adjacent :: Cell -> [Cell]
        adjacent c = [c' | c'<-startingCells, isAdjacent c c']

startingBoard :: Board 
startingBoard = b 
    where 
        (b, _, _) = graphFromEdges startingEdges
--------------------------------------------
---- End of Main.hs in the main package ----
-------------------------------------------- 

startCell :: Cell
startCell = ('a', 1)

n :: Int
n = 5

-- for example a1 -> a2 -> a3 -> a4 -> a5 => 4 arrows
testAStarInitial :: Test
testAStarInitial = TestCase (assertEqual "aStar initial setting" (fromJust (aStar startCell board n)) 4)
    where
        board = startingBoard

-- for example a1 -> a2 -> a3 -> a4 -> b4 -> c4 -> c5 => 6 arrows
testAStarOneWall :: Test
testAStarOneWall = TestCase (assertEqual "aStar one wall" (fromJust (aStar startCell board n)) 6)
    where
        board = placeWall startingBoard (wallTop ('a', 4))

multipleWallsBoard :: Board
multipleWallsBoard = foldl placeWall startingBoard [wallTop ('a', 4), wallTop ('c', 2), wallTop ('d', 4), wallRight ('b', 3)]

-- for example a1 -> a2 -> b2 -> c2 -> d2 -> e2 -> e3 -> e4 -> d4 -> c4 -> c5 => 10 arrows
testAStarMultipleWalls :: Test
testAStarMultipleWalls = TestCase (assertEqual "aStar multiple wall" (fromJust (aStar startCell multipleWallsBoard n)) 10)

-- for example a5 -> b5 -> c5 -> c4 -> c3 -> d3 -> e3 -> e2 -> e1 => 8 arrows
testAStarOtherSide :: Test
testAStarOtherSide = TestCase(assertEqual "aStar other side multiple walls" (fromJust (aStar ('a', 5) multipleWallsBoard 1)) 8)

-- All unit tests together
aStarTests :: Spec
aStarTests = fromHUnitTest $
    TestList [
        TestLabel "testAStarInitial" testAStarInitial,
        TestLabel "testAStarOneWall" testAStarOneWall,
        TestLabel "testAStarMultipleWalls" testAStarMultipleWalls,
        TestLabel "testAStarOtherSide" testAStarOtherSide]