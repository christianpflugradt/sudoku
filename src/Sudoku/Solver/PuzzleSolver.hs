module Sudoku.Solver.PuzzleSolver
  ( SolveResult (..),
    solve,
    solveWith,
  )
where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Grid
  ( Grid,
    isComplete,
  )
import Sudoku.Solver.HiddenSingleStrategy
  ( hiddenSingleStrategy,
  )
import Sudoku.Solver.NakedSingleStrategy
  ( nakedSingleStrategy,
  )
import Sudoku.Solver.Strategy
  ( AfterStep (..),
    PuzzleError (..),
    Strategy,
  )

----------------------------------------------------------------------
-- Public Types
----------------------------------------------------------------------

data SolveResult
  = Solved Grid
  | Unsolvable Grid
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

solve :: Grid -> Either PuzzleError SolveResult
solve = solveWith strategies

solveWith :: [Strategy] -> Grid -> Either PuzzleError SolveResult
solveWith strats grid =
  if isComplete grid
    then Right (Solved grid)
    else case step strats grid of
      Left err -> Left err
      Right Stuck -> Right (Unsolvable grid)
      Right (Progress updated) -> solveWith strats updated

----------------------------------------------------------------------
-- Internal Helpers
----------------------------------------------------------------------

step :: [Strategy] -> Grid -> Either PuzzleError AfterStep
step strats grid = go strats
  where
    go [] = Right Stuck
    go (strategy : remaining) =
      case strategy grid of
        Left err -> Left err
        Right (Progress updated) -> Right (Progress updated)
        Right Stuck -> go remaining

strategies :: [Strategy]
strategies =
  [ hiddenSingleStrategy,
    nakedSingleStrategy
  ]
