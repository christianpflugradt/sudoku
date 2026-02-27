module Sudoku.Solver.Strategy
  ( AfterStep (..),
    PuzzleError (..),
    Strategy,
  )
where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Grid (Grid)

----------------------------------------------------------------------
-- Public Types
----------------------------------------------------------------------

data AfterStep
  = Progress Grid
  | Stuck

data PuzzleError
  = Contradiction
  deriving (Eq, Show)

type Strategy = Grid -> Either PuzzleError AfterStep
