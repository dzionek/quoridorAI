# Quoridor in Haskell 

### Installation instructions 

You need GHC, the Cabal build system and the Stack tool. See [https://www.haskell.org/platform/](https://www.haskell.org/platform/). 

### Playing the game 

The easiest way to play the game is to go to the `src` directory, run `ghci Main` and execute the `main` function.

### Run the tests 

There are two test suites:
* Basic tests (`stack test :basic-tests`).
* Minimax tests (`stack test :minimax-tests`).

## Extensions
I added a couple of extensions to improve the Quoridor and its AI.

### A*
I have implemented A* available in the `AStar.hs` module. The algorithm uses a priority queue to schedule nodes according to:
* cost needed to go to that node, plus
* heuristic - straight vertical distance to a winning row.

To optimize further, the already visited nodes are stored in a set and checked before visiting again.

The queue is implemented using the `heaps` package from Hackage. This package is included in the cabal file. You may however need to install
it on your machine:

```bash
cabal install heaps
```

My implementation adds additional types in `Types.hs` and is used in the utility function.
### Unit tests for A*
To check the correctness of my A* implementation (in particualr its optimality), I added a new test suite with unit tests.
Check `tests/Extension/AStarTest.hs` and run:

```bash
stack test :extension-tests
```

### Position Evaluator
I added a module `Evaluator.hs` in which you can type any (partial) game and check the evaluation of its final position.
The module gives:
* static evaluation - evaluation with regard to just the final position (i.e. utility from `Minimax.hs`)
* dynamic evaluation - lookahead evaluation using Minimax

The game need to be written in a comma-separated format, e.g. "e2,d9,e3,c9". Check Reed.txt for more examples.

### Better utility
My extended utility is calculated not only with regard to the current player, but also the opponent. If you want to play my Minimax,
or use my `Evaluator.hs` please use it instead of the basic one.

### Safer game
I made sure that no player can be cut off in the game. To do this I added `playable` function in `Game.hs` and extended the
`validWallAction` function.

### Unit tests for lowFirst
Since I use lowFirst throughout my implementation, I changed the given highFirst unit tests to be relevant to lowFirst ordering.
See `tests/Minimax/OrderingTreeTest.hs`