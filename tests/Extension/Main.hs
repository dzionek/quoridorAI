module Main where 

import Test.Hspec

import AStarTest

main :: IO ()
main = hspec $ do
    describe "AStar tests" aStarTests 
