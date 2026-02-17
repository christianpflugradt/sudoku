module Sudoku.Placements
  ( Placements
  , PlacementError(..)
  ) where

import Sudoku.Grid
  ( Coordinate
  )

import Sudoku.Symbols
  ( Symbol
  )

----------------------------------------------------------------------
-- * Public API
----------------------------------------------------------------------

type Placements = [(Coordinate, Symbol)]

data PlacementError = PlacementError
  deriving(Eq, Show)
