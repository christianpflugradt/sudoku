module Sudoku.PuzzleSolver (PuzzleError (..), SolveResult (..), solve) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Grid
  ( Grid,
    isComplete,
  )

----------------------------------------------------------------------
-- Public Types
----------------------------------------------------------------------

data PuzzleError
  = Contradiction
  deriving (Eq, Show)

data SolveResult
  = Solved Grid
  | Unsolvable Grid
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

solve :: Grid -> Either PuzzleError SolveResult
solve grid =
  if isComplete grid
    then Right (Solved grid)
    else case step solvingStrategies grid of
      Left err -> Left err
      Right Stuck -> Right (Unsolvable grid)
      Right (Progress updated) -> solve updated

----------------------------------------------------------------------
-- Internal Helpers
----------------------------------------------------------------------

type SolvingStrategy = Grid -> Either PuzzleError AfterStep

data AfterStep
  = Progress Grid
  | Stuck

solvingStrategies :: [SolvingStrategy]
solvingStrategies = [alwaysStuckStrategy]

alwaysStuckStrategy :: SolvingStrategy
alwaysStuckStrategy _ = Right Stuck

step :: [SolvingStrategy] -> Grid -> Either PuzzleError AfterStep
step strategies grid = go strategies
  where
    go [] = Right Stuck
    go (strategy : remaining) =
      case strategy grid of
        Left err -> Left err
        Right (Progress updated) -> Right (Progress updated)
        Right Stuck -> go remaining
