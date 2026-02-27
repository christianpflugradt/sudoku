module Sudoku.Solver.Strategy
  ( AfterStep (..),
    PuzzleError (..),
    Strategy,
    strategies,
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

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

strategies :: [Strategy]
strategies = []
