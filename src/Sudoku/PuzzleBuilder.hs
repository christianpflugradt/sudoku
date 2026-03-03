module Sudoku.PuzzleBuilder
  ( BuildError (..),
    buildPuzzle,
  )
where

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

data BuildError
  = InvalidGridShape
  | PlacementFailure PlacementError
  deriving (Eq, Show)

buildPuzzle :: Symbols -> Placements -> Either BuildError Grid
buildPuzzle symbols placements =
  case emptyGrid symbols of
    Nothing -> Left InvalidGridShape
    Just grid -> foldM step grid placements
  where
    step :: Grid -> (Coordinate, Symbol) -> Either BuildError Grid
    step grid (coord, symbol) =
      case setCell grid coord symbol of
        Left err -> Left (PlacementFailure err)
        Right updated -> Right updated
