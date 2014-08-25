# John Conway's Game of Life

The game of life is a simulation of cellular automaton.  Each cell is arranged in a 2-D grid and has 8 neighboring
cells.  The life cycle of a cell is dictated by a simple set of rules.

* If a cell is alive and has fewer than 2 neighbors, it dies as if by loneliness.
* If a cell is alive and has more than 3 neighbors, it dies as if by starvation due to over-crowding.
* If a cell is alive and has 2 or 3 neighbors it remains alive.
* If a cell is dead and has exactly 3 neighbors it spawns, as if by reproduction.

A further description of the game of life can be found at [Wikipedia](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

## [Haskell Optimization and the Game of Life](http://blog.headcrab.info/haskell-optimization-and-the-game-of-life/)
I've written a short paper on the process I went through to optimize this program.  I hope it helps out
anyone looking to learn how to make Haskell software fast.

## Build Instructions
Requires GHC 7.6, best results with GHC 7.8.  Additional dependencies are listed in gameoflife.cabal.

* `cabal install -j`
* `cabal configure -f llvm` (Optional, if you have llvm installed)

## Instructions
Instructions for using the life program once installed.

```
The life program
  
life [OPTIONS]
  
Common flags:
  -w --width=INT      The number of cells across the game board.
  -h --height=INT     The number of cells tall.
  -c --cellwidth=INT  The number of pixels across a single cell.
  -g --genpersec=INT  The number of generations per second.
  -? --help           Display help message
  -V --version        Print version information
```

For example, if you wanted a game of life where the board was 100x100 and the 20 generations occurred every
second, you would enter `life -w 100 -h 100 -g 20` in the command line.  The cell width modifier allows you 
to change the scale of each cell in pixels so that you can keep the whole game board on screen.
