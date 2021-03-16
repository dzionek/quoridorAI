# Quoridor in Haskell 

### Installation instructions 

You need GHC, the Cabal build system and the Stack tool. See [https://www.haskell.org/platform/](https://www.haskell.org/platform/). 

### Playing the game 

The easiest way to play the game is to go to the `src` directory, run `ghci Main` and execute the `main` function.

### Run the tests 

There are two test suites:
* Basic tests (`stack test :basic-tests`).
* Minimax tests (`stack test :minimax-tests`).

## Extension
I added a couple of extensions to improve the Quoridor and its AI.
* Much better utility function - implements A* to find the shortest paths (see `AStar.hs`)
* Added unit tests to prove that my A* implementation is correct and yields the shortest paths (test suite `stack test :extension-tests`)
* Since I used lowFirst throughtout my implementation, I changed the 
