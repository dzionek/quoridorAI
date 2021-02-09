{- 
    Module: Print.

    This file contains all the functions used to display the board in the command line. We only use
    'printGame' in the main game loop in Main.hs. If you want to modify the appearance of the game,
    you should be able to do so by modifying this file.
-}
module Print where

import Data.Array

import Types
import Constants
import Cell
import Player

-- String representation of a cell (('a', 1) -> "a1", ...).
cellToString :: Cell -> String 
cellToString (i, j) = [i] ++ (show j)

-- If the two cells are connected (in the board), return s otherwise return " ".
printIfConn :: Board -> Cell -> Cell -> String -> String 
printIfConn b c c' s
    | c' `elem` nbhd = s 
    | otherwise = " "
        where 
            nbhd = [indexToCell j | j<-(b!(cellToIndex c))]

-- Given a cell and maybe a player print either the cell name or the player's name.
printPlayer :: Cell -> Maybe Player -> String  
printPlayer c Nothing = " " ++ (cellToString c)
printPlayer _ (Just p) = " " ++ (name p) ++ " "

-- Print whole cell. Consists of three lines. They contain both the content of the cell and the 
-- connections to the surrounding cells in the board.
printCell :: Board -> [Player] -> Cell -> (String, String, String)
printCell b ps c = 
    let left = printIfConn b c (cellLeft c) "-" in 
    let right = printIfConn b c (cellRight c) "-" in
    let top = printIfConn b c (cellTop c) "|" in 
    let bottom = printIfConn b c (cellBottom c) "|" in 
    let player = printPlayer c $ playerInCell ps c in 
    ("  " ++ top ++ "  ", left ++ player ++ right, "  " ++ bottom ++ "  ")

-- Concatenates all the cells in a row to form a string representing the row.
printRow :: Board -> [Player] -> Row -> String
printRow b ps j = tops ++ "\n" ++ mids ++ "\n" ++ bots
    where 
        (tops, mids, bots) = foldl 
            (\(at, am, ab) (t, m, b) -> (at ++ t, am ++ m, ab ++ b)) 
            ("", "", "") 
            [(printCell b ps (i, j)) | i<-allColumns]

-- Concatenates all the rows to form a string representing the board.
printBoard' :: Board -> [Player] -> String
printBoard' b ps = concat [(printRow b ps r) ++ "\n" | r<-reverse allRows]

-- IO version of the function above.
printBoard :: Board -> [Player] -> IO ()
printBoard b ps = putStr $ printBoard' b ps

-- Shortcut for printBoard.
printGame :: Game -> IO ()
printGame (Game b ps) = printBoard b ps 
