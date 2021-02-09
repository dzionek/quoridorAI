{-
    Module: Cell

    This file contains operations on cells. 
-}
module Cell where

import Data.Char 

import Types
import Constants

{-
    Basic operations on columns.
-}

-- Convert column to integer ('a' -> 1, 'b' -> 2, ...).
columnToInt :: Column -> Int 
columnToInt c = (ord c) - (ord 'a') + 1

-- Convert integer to column (1 -> 'a', 2 -> 'b', ...).
intToColumn :: Int -> Column 
intToColumn i = chr ((ord 'a') + i - 1)

-- Return column on the left ('b' -> 'a', 'c' -> 'b', ...).
columnLeft :: Column -> Column 
columnLeft c = intToColumn ((columnToInt c) - 1)

-- Return column on the right ('a' -> 'b', 'b' -> 'c', ...).
columnRight :: Column -> Column  
columnRight c = intToColumn ((columnToInt c) + 1)

{-
    Surrounding cells.
-}

-- Cell on the left.
cellLeft :: Cell -> Cell 
cellLeft (i, j) = (columnLeft i, j)

-- Cell on the right.
cellRight :: Cell -> Cell 
cellRight (i, j) = (columnRight i, j)

-- Cell on the top.
cellTop :: Cell -> Cell 
cellTop (i, j) = (i, j + 1)

-- Cell on the bottom.
cellBottom :: Cell -> Cell 
cellBottom (i, j) = (i, j - 1)

-- All surrounding cells.
cellsAround :: Cell -> [Cell]
cellsAround c = [cellLeft c, cellRight c, cellTop c, cellBottom c]

{-
    Cells in board. 
    
    Note that so far we haven't checked if the cell is actually in the board, we do
    so using the following functions.
-}

-- Checks if cell is within bounds using 'boardSize' from Constants.hs.
cellInBoard :: Cell -> Bool 
cellInBoard (i, j) = 
    (columnToInt i) >= 1 && (columnToInt i) <= boardSize && j >= 1 && j <= boardSize

-- List of cells around a cell that are contained in the board.
cellsAroundInBoard :: Cell -> [Cell]
cellsAroundInBoard c = filter cellInBoard (cellsAround c)

{-
    Distance between cells and adjacency. 
-}

-- 'Taxicab' distance between cells.
distance :: Cell -> Cell -> Int 
distance (i, j) (i', j') = abs ((columnToInt i') - (columnToInt i)) + abs (j' - j)

-- A quick way of computing: c `elem` (cellsAround c').
isAdjacent :: Cell -> Cell -> Bool 
isAdjacent c c' = distance c c' == 1

-- Two cells are next to each other in the same row.
isHorizontallyAdjacent :: Cell -> Cell -> Bool 
isHorizontallyAdjacent c c' = (c == cellLeft c') || (c == cellRight c')

-- Two cells are next to each other in the same column.
isVerticallyAdjacent :: Cell -> Cell -> Bool 
isVerticallyAdjacent c c' = (c == cellBottom c') || (c == cellTop c')

{-
    Going from cells to indices (nodes in a Graph) and backwards.
-}

-- Convert cell to index (('a', 1) -> 0, ('a', 2) -> 1, ...).
cellToIndex :: Cell -> Index 
cellToIndex (i, j) = boardSize * ((columnToInt i) - 1) + (j - 1)

-- Convert index to cell (0 -> ('a', 1), 1 -> ('a', 2), ...).
indexToCell :: Index -> Cell 
indexToCell idx = (intToColumn (i + 1), j + 1) 
    where 
        (i, j) = divMod idx boardSize
