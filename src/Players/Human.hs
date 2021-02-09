{-
    Module: Human.

    This is the non-AI type of player. It chooses its actions according to the user input, following
    the standard notation of the game.
-}
module Players.Human where 

import Data.Char
import Data.String

import Types
import Action 
import Cell 
import Player
import Board

-- Translates commands to actions. There are two types:
-- * Move commands of the form 'c2', meaning 'move to c2'.
-- * Place commands of the form 'c3v' meaning 'place a vertical wall next to c3' (similarly 'h').
commandToAction :: Board -> [Player] -> String -> Int -> Maybe Action
commandToAction _ (p:_) [i, j] _ = Just (Move (currentCell p, (i, digitToInt j)))
commandToAction _ (p:_) [i, j, d] _
    | d == 'h' = Just (Place (wallTop (i, digitToInt j)))
    | d == 'v' = Just (Place (wallRight (i, digitToInt j)))
    | otherwise = Nothing 
commandToAction _ _ _ _ = Nothing

-- We build a human player from a name, a starting cell, a number of walls, an array of winning
-- positions and 'commandToAction'.
makeHumanPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeHumanPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = True,
    chooseAction = commandToAction }
