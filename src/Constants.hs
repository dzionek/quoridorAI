{-
    Module: Constants.

    We define some global constants (board size, number of players and total walls) and calculate
    some useful related values.
-}
module Constants where

import Data.Char

import Types

{- 
    Actual global constants.
-}

-- Board size. Everything should work by simply changing this number, however, we recommend that you
-- keep it small to simplify the calculations. It should be odd and at least 3.
boardSize :: Int
boardSize = 5

-- Number of players.
numOfPlayers :: Int
numOfPlayers = 2

-- Total number of walls allowed.
totalWalls :: Int
totalWalls = 10

{-
    Other variables.
-}

-- First row. Note that this will be the row at the bottom of the board.
firstRow :: Row
firstRow = 1

-- Last row. Note that this will be the row at the top of the board.
lastRow :: Row 
lastRow = boardSize

-- Array with all the row indices.
allRows :: [Row]
allRows = [firstRow..lastRow]

-- First column (always 'a').
firstColumn :: Column 
firstColumn = 'a' 

-- Last column (if boardSize = 5 this would be 'e').
lastColumn :: Column 
lastColumn = chr (ord 'a' + boardSize - 1)

-- Array with all the column names.
allColumns :: [Column]
allColumns = [firstColumn..lastColumn]

-- We split the walls evenly by default.
wallsPerPlayer :: Int 
wallsPerPlayer = div totalWalls numOfPlayers
