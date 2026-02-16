module Sudoku.Placements
  ( Placements
  , PlacementError(..)
  ) where

import Sudoku.Grid (Coordinate, Symbol)

----------------------------------------------------------------------
-- * Public API
----------------------------------------------------------------------

type Placements = [(Coordinate, Symbol)]

data PlacementError = PlacementError
  deriving(Eq, Show)
