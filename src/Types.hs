{-
    Module: Types.

    All the new types defined are found here. It should serve as a quick reference.
-}
module Types where 

import Data.Graph

{-
    Also see module Cell.
-}

-- Each node in the graph is represented by an index, which is an integer.
type Index = Int

-- A cell is a pair (column, row). Columns are indexed by characters ('a', 'b', 'c', ...) and rows 
-- are indexed by numbers starting from 1 (1, 2, 3, ...).
type Column = Char
type Row = Int 
type Cell = (Column, Row)

{-
    Also see module Board.
-}

-- The board is represented by a graph. 
-- (See https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Graph.html).
type Board = Graph

{-
    Also see module Action.
-}

-- A step is a pair of two cells representing a movement from one cell to the other.
type Step = (Cell, Cell)
-- A wall is a pair of two steps representing the two steps that it blocks.
type Wall = (Step, Step)
-- An action is either "moving a step" or "placing a wall".
data Action = Move Step | Place Wall deriving (Show)

{- 
    Also see module Player. 
-}

-- A player is the following structure.
data Player = Player { 
    -- Name of the player, will be printed when playing.
    name :: String, 
    -- Current turn in the game.
    turn :: Int,
    -- Current cell occupied by the player.
    currentCell :: Cell,
    -- Number of remaining walls.
    remainingWalls :: Int,
    -- List of cells corresponding to the winning positions.
    winningPositions :: [Cell],
    -- Is it a human player? If it is, you will be asked to write a command when it's your turn, and
    -- if it isn't you won't.
    isHuman :: Bool,
    -- Key function. Given a game state and a command, a player has to come up with an action.
    -- The last parameter is an integer that can be used to pass a random number from the main game
    -- loop, which might be necessary for certain algorithms.
    chooseAction :: Board -> [Player] -> String -> Int -> Maybe Action }

{-
    Also see module Game.
-}

-- A game holds a board and a list of players.
data Game = Game Board [Player]

{-
    Also see Players.MinimaxPlayer.
-}

-- Multi-branching tree that holds a value in each node and each edge.
data StateTree v a = StateTree v [(a, StateTree v a)]

-- Tree representing game states.
type GameTree = StateTree Game Action
-- Tree representing scores.
type EvalTree = StateTree Int Action

-- Data type that holds both the score and a list of actions (associated to that score).
data Result = Result !Int [Action]

-- We can equate results by looking at the score.
instance Eq Result where 
    (Result x _) == (Result y _) = x == y 

-- We can compare results by looking at the score.
instance Ord Result where 
    compare (Result x _) (Result y _) = compare x y
