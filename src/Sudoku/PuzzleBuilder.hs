module Sudoku.PuzzleBuilder
  ( buildPuzzle
  ) where

import Control.Monad (foldM)

import Sudoku.Grid
  ( Placements
  , PlacementError
  , Coordinate
  , Grid
  , emptyGrid
  , setCell
  )

import Sudoku.Symbols
  ( Symbol
  , Symbols
  )

----------------------------------------------------------------------
-- * Public API
----------------------------------------------------------------------

buildPuzzle :: Symbols -> Placements -> Either PlacementError Grid
buildPuzzle symbols placements =
  case emptyGrid symbols of
    Nothing -> error "unreachable: emptyGrid must succeed"
    Just grid -> foldM step grid placements
  where
    step :: Grid -> (Coordinate, Symbol) -> Either PlacementError Grid
    step grid (coord, symbol) = setCell grid coord symbol
