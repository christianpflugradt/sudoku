module Sudoku.Placements (PlacementError (..), Placements) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Geometry (Coordinate)
import Sudoku.Symbols (Symbol)

----------------------------------------------------------------------
-- Public Types
----------------------------------------------------------------------

type Placements = [(Coordinate, Symbol)]

data PlacementError
  = OutOfBounds
  | AlreadySet
  | DuplicateInUnit
  | NoCandidates
  deriving (Eq, Show)
