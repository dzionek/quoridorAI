{-
    Module: A Star
    Created as an extension.
-}
module AStar where

import qualified Data.Heap as H
import qualified Data.Set as Set

import Types
import Cell
import Board

getWiningRow :: Player -> Row
getWiningRow p = snd $ head $ winningPositions p

-- Heuristic for A*, uses vertical straight line distance to winning positions.
verticalHeuristic :: Cell -> Row -> Int
verticalHeuristic (_, row) winningRow = abs $ winningRow - row


-- A* algorithm returning distance to one winning positions.
aStar :: Cell -> Board -> Row -> Maybe Cost
aStar cell = aStar' Nothing queue
    where
        -- Min-heap priority queue with priority cost+heuristic and payload cell + cost + history
        -- See Types.hs for reference.
        queue :: PriorityQueue
        queue = H.singleton $ H.Entry 0 (cell, 0, Set.empty)

aStar' :: Maybe Cost -> PriorityQueue -> Board -> Row -> Maybe Cost
aStar' (Just cost) _ _ _ = Just cost
aStar' Nothing queue b winningRow
    | H.null queue = Nothing
    | otherwise =
        if Set.member (cellToIndex (col, row)) history
        then aStar' Nothing (H.deleteMin queue) b winningRow
        else
            if row == winningRow
            then aStar' (Just dist) queue b winningRow
            else aStar' Nothing (addNextNodes (col, row) dist history (H.deleteMin queue) b winningRow) b winningRow
    where
        ((col, row), dist, history) = H.payload $ H.minimum queue
        
addNextNodes :: Cell -> Cost -> History -> PriorityQueue -> Board -> Row -> PriorityQueue
addNextNodes cell cost history queue b winningRow = foldr H.insert queue nextEntries
    where
        history' :: History
        history' = Set.insert (cellToIndex cell) history

        nextEntries :: [H.Entry Int Payload]
        nextEntries = [ H.Entry (cost+1 + verticalHeuristic c winningRow) (c, cost+1, history')| c <- reachableCells b cell ]