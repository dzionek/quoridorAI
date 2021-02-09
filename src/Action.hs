{-
    Module: Action.

    In this file we list some basic functions involving the 'Step', 'Wall' and 'Action' types.
-}
module Action where

import Types
import Cell

-- Needed to be able to compare actions. The case of wall equality is more complex because there are
-- different combinations of steps that may represent the same wall.
instance Eq Action where 
    a1 == a2 = case (a1, a2) of
        (Move s, Move s') -> s == s'
        (Place ((c1, c2), (c3, c4)), Place ((c1', c2'), (c3', c4'))) -> 
            (c1 == c1' && c2 == c2' && c3 == c3' && c4 == c4') ||
            (c1 == c1' && c2 == c2' && c3 == c4' && c4 == c3') ||
            (c1 == c2' && c2 == c1' && c3 == c3' && c4 == c4') || 
            (c1 == c2' && c2 == c1' && c3 == c4' && c4 == c3')
        _ -> False

{-
    Step util.
-}

-- Convert step to graph edge.
stepToEdge :: Step -> (Index, Index)
stepToEdge (c, c') = (cellToIndex c, cellToIndex c')

-- Step to the left.
stepLeft :: Cell -> Step 
stepLeft c = (c, cellLeft c)

-- Step to the right.
stepRight :: Cell -> Step 
stepRight c = (c, cellRight c)

-- Step to the top.
stepTop :: Cell -> Step 
stepTop c = (c, cellTop c)

-- Step to the bottom.
stepBottom :: Cell -> Step 
stepBottom c = (c, cellBottom c)

-- All steps.
allStepsFrom :: Cell -> [Step]
allStepsFrom c = [stepLeft c, stepRight c, stepTop c, stepBottom c]

-- Make steps.
makeSteps :: Cell -> [Cell] -> [Step]
makeSteps c cs = [(c, c') | c'<-cs]

{-
    Wall util. Picture of walls surrounding cell c:
 
            * <- right
            *
        * * * * * <- top
        * c *
    * * * * * <- bottom
        *   
        * <- left  
-}

-- Vertical wall on the left.
wallLeft :: Cell -> Wall 
wallLeft c = (stepLeft c, stepLeft (cellBottom c))

-- Vertical wall on the right (used for commands ending in 'v').
wallRight :: Cell -> Wall 
wallRight c = (stepRight c, stepRight (cellTop c))

-- Horizontal wall on the top (used for commands ending in 'h').
wallTop :: Cell -> Wall 
wallTop c = (stepTop c, stepTop (cellRight c))

-- Horizontal wall on the bottom.
wallBottom :: Cell -> Wall 
wallBottom c = (stepBottom c, stepBottom (cellLeft c))

-- All walls around a cell.
allWallsAround :: Cell -> [Wall]
allWallsAround c = [wallLeft c, wallRight c, wallTop c, wallBottom c]
