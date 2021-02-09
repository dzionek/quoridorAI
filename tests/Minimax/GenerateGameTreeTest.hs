{-
    Module: GenerateGameTreeTest

    Used to test Part I.a.
-}
module GenerateGameTreeTest where

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Data.Graph
import Data.List

import Types
import Constants
import Cell
import Game
import Players.Minimax
import Util.GameInstances

{-
    Util.
-}

-- Apply 'performAction' successively.
performActions :: Game -> [Action] -> Maybe Game
performActions g [] = Just g 
performActions g (a:as) = case (performAction g a) of 
    Just g' -> performActions g' as 
    Nothing -> Nothing

-- Use a list (of indices) to traverse a game tree. Also keep track of the actions that let you to
-- that game state.
traverseGameTree :: [Int] -> GameTree -> (Game, [Action])
traverseGameTree = traverseGameTree' []
    where 
        traverseGameTree' :: [Action] -> [Int] -> GameTree -> (Game, [Action])
        traverseGameTree' acts [] (StateTree g _) = (g, acts) 
        traverseGameTree' acts _ (StateTree g []) = (g, acts) 
        traverseGameTree' acts (i:is) (StateTree _ ts) = 
            traverseGameTree' (acts ++ [fst p]) is (snd p) 
            where 
                p :: (Action, GameTree) 
                p = ts!!(i `mod` (length ts))

{-
    Some defaults to be able to generate games.
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
    QuickCheck test.

    For this part, there is only one test that randomly traverses the game tree and checks that the
    game state reached is the correct one.
-}

-- The game tree should be generated in a way such that if we perform all the actions in the edges
-- we get to the same game state.
testGenerateGameTreeQuickCheck :: [Int] -> Expectation
testGenerateGameTreeQuickCheck is = case (traverseGameTree is gt) of 
    (g, acts) -> assertEqual "Just g" (Just g) (performActions startingGame acts)
    where 
        gt :: GameTree
        gt = generateGameTree startingGame

-- The QuickCheck test as a Spec.
generateGameTreeQuickCheckTest :: Spec
generateGameTreeQuickCheckTest = do
    prop "testGenerateGameTreeQuickCheck" testGenerateGameTreeQuickCheck
