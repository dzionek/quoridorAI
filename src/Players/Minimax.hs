{-
    Module: Minimax.

    *** PART I (60pt) and PART II (10pt) *** 
-}
module Players.Minimax where 

import Data.Maybe
import Data.Graph
import Data.Ord
import Data.Tree
import Data.List
import Data.Array

import Types
import Constants
import Cell
import Action
import Board 
import Player
import Game
import Players.Dumb (dumbAction)

-- Additional imports
import qualified Data.Heap as H
import qualified Data.Set as Set

{-
    StateTree util.
-}

-- Map a function through the nodes of the tree.
mapStateTree :: (v -> w) -> StateTree v a -> StateTree w a
mapStateTree f (StateTree x ts) = StateTree (f x) [(a, mapStateTree f t) | (a, t)<-ts]

-- Calculate the depth of the tree (used to test pruneDepth).
stateTreeDepth :: StateTree v a -> Int 
stateTreeDepth (StateTree _ []) = 0
stateTreeDepth (StateTree _ ts) = 1 + (maximum (map (stateTreeDepth . snd) ts))

-- Calculate the breadth of the tree (used to test pruneBreadth).
stateTreeBreadth :: StateTree v a -> Int
stateTreeBreadth (StateTree _ []) = 0
stateTreeBreadth (StateTree _ ts) = max (length ts) (maximum (map (stateTreeBreadth . snd) ts))

{-
    Result util.
-}

-- Negating the result is simply negating the score. You may ignore this although it may be useful
-- to implement the minimax algorithm.
negResult :: Result -> Result
negResult (Result x as) = Result (-x) as

{- 
    *** Part I.a (10pt) ***

    First, we will generate a tree containing all the possible game states.
-}

-- Given a game, return a tree that encodes all the possible future game states.
-- [Hint: Use 'validActions' and 'performAction'.]
-- [Note: To speed things up, you may want to, at this stage, heuristically select which actions are 
--  more relevant. In particular, you probably don't want to consider every single possible wall.]
generateGameTree :: Game -> GameTree
generateGameTree g = StateTree g deeperTree
    where
        deeperTree = [ (a, generateGameTree $ fromJust nextGame)
                        | a <- validActions g,
                        let nextGame = performAction g a,
                        isJust nextGame ]

{-
    *** PART I.b (5pt) ***

    Re-order the tree so that when traversed by the minimax algorithm, when it traverses the 
    branches at each node, finds either the higher scores or the lower scores first, depending on
    the depth of the tree.
-}

-- Sort tuples from the lowest to the highest
sortTuples :: (Ord v) => [(a, StateTree v a)] -> [(a, StateTree v a)]
sortTuples = sortBy (\(a, StateTree v1 a') (b, StateTree v2 b') -> compare v1 v2)

-- Higher scoring nodes go first.
-- [Hint: You should use 'lowFirst'.]
highFirst :: (Ord v) => StateTree v a -> StateTree v a
highFirst (StateTree v ts) = StateTree v [ (a, lowFirst t)
                                            | (a, t) <- reverse $ sortTuples ts ] 

-- Lower scoring nodes go first.
-- [Hint: You should use 'highFirst'.]
lowFirst :: (Ord v) => StateTree v a -> StateTree v a
lowFirst (StateTree v ts) = StateTree v [ (a, highFirst t)
                                            | (a, t) <- sortTuples ts ] 

{-
    *** Part I.c (5pt) ***

    We don't want to look at all possible future game states as that would consume too much time and
    memory. Instead, we will only look a given number of steps into the future. Formally, the future
    game states are encoded in a tree, so we need a function that reduces the depth of a tree.
-}

-- Given a depth and a tree, return the same tree but cutting off the branches when the depth is 
-- exceeded. 
-- [Hint: You may want to use guards and recursion.]
pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth n (StateTree v ts)
    | n >= 1 = StateTree v [(a, pruneDepth (n-1) subTree) | (a, subTree) <- ts]
    | otherwise = StateTree v []

{-
    *** Part I.d (5pt) ***

    Similarly, we can also make our tree smaller by not considering all the possible game states at
    a given point. We need a function that reduces the breadth (or width) of a tree.
-}

-- Given a breadth (Int n) and a tree, return the same tree but only keeping the first n branches at
-- every node. 
-- [Hint: Use 'take'.]
pruneBreadth :: Int -> StateTree v a -> StateTree v a
pruneBreadth n (StateTree v ts) =  StateTree v [ (a, pruneBreadth n subTree)
                                                    | (a, subTree) <- take n ts ]

{-
    *** Part I.e (15pt) ***

    A crucial part of the minimax algorithm is defining a good utility function. It should measure
    how good a game position is for the current player. In our case, a game state should be better
    than another one if the player is closer to its winning positions.
-}

-- Assign a value to each game (from the point of view of the current player).
-- [Hint 1: You may want to calculate the distance between the player's current cell and its winning
--  positions.]
-- [Hint 2: One way would be to use 'reachableCells' repeatedly.]

-- Heuristic for A*, uses vertical straight line distance to winning positions.
verticalHeuristic :: Cell -> Int
verticalHeuristic (_, row) = boardSize - row


-- A* algorithm returning distance to one winning positions.
aStar :: Board -> Cell -> Maybe Cost
aStar b cell = aStar' Nothing queue b 
    where
        -- Min-heap priority queue with priority cost+heuristic and payload cell + cost + history
        -- See Types.hs for reference.
        queue :: PriorityQueue
        queue = H.singleton $ H.Entry 0 (cell, 0, Set.empty)

aStar' :: Maybe Cost -> PriorityQueue -> Board -> Maybe Cost
aStar' (Just cost) _ _ = Just cost
aStar' Nothing queue b
    | H.null queue = Nothing
    | otherwise =
        if Set.member (cellToIndex (col, row)) history
        then aStar' Nothing (H.deleteMin queue) b
        else
            if row == boardSize
            then aStar' (Just dist) queue b
            else aStar' Nothing (addNextNodes (col, row) dist history (H.deleteMin queue) b) b
    where
        ((col, row), dist, history) = H.payload $ H.minimum queue
        
addNextNodes :: Cell -> Cost -> History -> PriorityQueue -> Board -> PriorityQueue
addNextNodes cell cost history queue b = foldr H.insert queue nextEntries
    where
        history' :: History
        history' = Set.insert (cellToIndex cell) history

        nextEntries :: [H.Entry Int Payload]
        nextEntries = [ H.Entry (cost+1 + verticalHeuristic c) (c, cost+1, history')| c <- reachableCells b cell ]

utility :: Game -> Int 
utility (Game b players) = -(fromJust $ aStar b playerCell)
    where
        player = currentPlayer players
        playerCell = currentCell player


-- Lifting the utility function to work on trees.
evalTree :: GameTree -> EvalTree 
evalTree = mapStateTree utility 

{-
    *** Part I.f (20pt) ***

    Finally, we ask you to implement the minimax algorithm. Given an evaluation tree, it should 
    return the a high scoring action (according to the minimax algorithm).
-}

-- Given an evaluation tree (it stores a score in the node and each branch is labelled with the 
-- action that leads to the next child) return a list of actions
-- [Hint 1: Use a helper function to keep track of the highest and lowest scores.]
-- [Hint 2: Use the 'Result' datatype.]
minimaxFromTree :: EvalTree -> Action
minimaxFromTree = undefined

{-
    *** Part II (10pt) ***

    Extension of Part I.e, using alpha-beta pruning. You will need to change the 'minimax' function
    below to make it use this function.
-}

-- Same as above but now use alpha-beta pruning.
-- [Hint 1: Extend the helper function in I.e to keep track of alpha and beta.]
-- [Hint 2: Use the 'Result' datatype.]
minimaxABFromTree :: EvalTree -> Action
minimaxABFromTree = undefined 

{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depth :: Int 
depth = 4

-- Given breadth for pruning.
breadth :: Int 
breadth = 10

-- Function that combines all the different parts implemented in Part I.
minimax :: Game -> Action
minimax =
      minimaxFromTree -- or 'minimaxABFromTree'
    . pruneBreadth breadth
    . highFirst
    . evalTree
    . pruneDepth depth
    . generateGameTree 

-- Given a game state, calls minimax and returns an action.
minimaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction b ps _ r = let g = Game b ps in minimaxAction' g (minimax g)
    where 
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make minimaxPlayer in the usual way using 'minimaxAction'.
makeMinimaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeMinimaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = minimaxAction }
