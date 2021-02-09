module Main where 

import Test.Hspec

import ActionTest
import CellTest
import BoardTest
import PlayerTest
import GameTest
import HumanTest

main :: IO ()
main = hspec $ do
    describe "Cell unit tests" $ cellUnitTests
    describe "Cell QuickCheck tests" $ cellQuickCheckTests
    describe "Action unit tests" $ actionUnitTests
    describe "Board unit tests" $ boardUnitTests
    describe "Player unit tests" $ playerUnitTests
    describe "Game unit tests" $ gameUnitTests
    describe "HumanPlayer unit tests" $ humanPlayerUnitTests
