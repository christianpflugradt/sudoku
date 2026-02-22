module Sudoku.PuzzleBuilder (buildPuzzle) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Monad (foldM)
import Sudoku.Geometry (Coordinate)
import Sudoku.Grid
  ( Grid,
    emptyGrid,
    setCell,
  )
import Sudoku.Placements
  ( PlacementError,
    Placements,
  )
import Sudoku.Symbols
  ( Symbol,
    Symbols,
  )

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

buildPuzzle :: Symbols -> Placements -> Either PlacementError Grid
buildPuzzle symbols placements =
  case emptyGrid symbols of
    Nothing -> error "unreachable: emptyGrid must succeed"
    Just grid -> foldM step grid placements
  where
    step :: Grid -> (Coordinate, Symbol) -> Either PlacementError Grid
    step grid (coord, symbol) = setCell grid coord symbol
