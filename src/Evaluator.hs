{-
    Module: Evaluator

    EXTENSION
    Used to evaluate the concrete final position specified as an input.
-}
module Evaluator where

import Data.List.Split (splitOn)
import Data.Char(digitToInt)
import Data.Maybe (fromJust)

import Players.Minimax (utility, minimaxScore)
import Cell (cellInBoard)
import Main (startingBoard, startingPlayersMiddle, nameToPlayerConstructor)
import Types
import Game (currentPlayer, performAction)
import Action (wallTop, wallRight)

-- Check if the string are actions, i.e. they represent a cell in the board and have valid length or format ('v' or 'h' for walls).
verifyActionsFormat :: [String] -> Bool
verifyActionsFormat ss = ofSizeTwoOrThree ss && inBoards ss && all validWalls ss
    where
        ofSizeTwoOrThree = all (\s -> length s >= 2 && length s <= 3)
        inBoards = all (\s -> cellInBoard (head s, digitToInt (s!!1)))
        validWalls s
            | length s == 3 = s!!2 == 'h' || s!!2 == 'v'
            | otherwise = True

-- Give the action represented by the string. E.g. 'a1' for the current player must move it to this position.
createAction :: String -> Cell -> Action
createAction s c
    | length s == 2 = Move (c, c')
    | otherwise     = Place (
        case s!!2 of
            'h' -> wallTop c'
            'v' -> wallRight c')
    where
        c' = (head s, digitToInt (s!!1))

-- Give the static and dynamic evaluations of the final position.
checkEvaluation :: [String] -> Game -> Maybe (Int, Int)
checkEvaluation commands g =
    case g' of
        Just g'' -> 
            case commands of
                [c] -> Just (utility g'', minimaxScore g'')
                (c:cs) -> checkEvaluation cs g''
        Nothing  -> Nothing
    where
        (Game b ps) = g
        p = currentPlayer ps
        playerCell = currentCell p
        action = createAction (head commands) playerCell
        g' = performAction g action

-- Run the evaluator.
main :: IO ()
main = do {
    putStrLn "Provide a (partial) game in the comma-separated algebraic format.";
    gameLine <- getLine;
    let actionSequence = splitOn "," gameLine in
        if not $ verifyActionsFormat actionSequence
            then do { putStrLn "The given string is in invalid format or out of the board. Try again!"; main }
            else 
                case checkEvaluation actionSequence g of
                    Just score -> putStrLn ("The evaluation of this final position is: static=" ++ show (fst score) ++ " dynamic=" ++ show (snd score) ++ ".")
                    Nothing -> putStrLn "This is not a valid game."
}
    where
        g = Game startingBoard (startingPlayersMiddle (fromJust (nameToPlayerConstructor "Minimax")) (fromJust (nameToPlayerConstructor "Minimax")))