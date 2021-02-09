module Main where 

import Test.Hspec

import GenerateGameTreeTest
import OrderingTreeTest
import PruneDepthTest
import PruneBreadthTest
import UtilityTest 

main :: IO ()
main = hspec $ do
    describe "Part I.a QuickCheck test" generateGameTreeQuickCheckTest
    describe "Part I.b unit tests" orderingTreeUnitTests
    describe "Part I.b QuickCheck tests" orderingTreeQuickCheckTests
    describe "Part I.c unit tests" pruneDepthUnitTests
    describe "Part I.c QuickCheck tests" pruneDepthQuickCheckTests
    describe "Part I.d unit tests" pruneBreadthUnitTests
    describe "Part I.d QuickCheck tests" pruneBreadthQuickCheckTests
    describe "Part I.e unit tests" utilityUnitTests
