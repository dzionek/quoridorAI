{-
    Module: Dumb.

    Implementation of a player that simply picks a random valid step.
-}
module Players.Dumb where 

import Data.List

import Types
import Constants
import Action 
import Board 
import Player
import Cell

-- All possible steps.
allSteps :: [Step]
allSteps = [(c1, c2) | c1<-allCells, c2<-allCells]
    where
        allCells = [(i, j) | i<-allColumns, j<-allRows]

-- Select random valid move.
dumbAction :: Board -> [Player] -> String -> Int -> Maybe Action
dumbAction b (p:ps) _ r = if validMoves == [] then Nothing else Just (validMoves!!randomIdx)
    where 
        validMoves = [Move mv | mv<-allSteps, (canMove p ps mv) && (validStep b mv)]
        randomIdx = r `mod` (length validMoves)

-- Make player in the usual way using 'dumbAction'.
makeDumbPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeDumbPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = dumbAction }
