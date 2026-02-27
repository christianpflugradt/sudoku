module Sudoku.Solver.NakedSingleStrategy
  ( nakedSingleStrategy,
  )
where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Sudoku.Geometry (Coordinate)
import Sudoku.Grid
  ( Cell (..),
    Grid,
    allCoordinates,
    cellAt,
    setCell,
  )
import Sudoku.Solver.Strategy
  ( AfterStep (..),
    PuzzleError (..),
    Strategy,
  )
import Sudoku.Symbols (Symbol)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

nakedSingleStrategy :: Strategy
nakedSingleStrategy grid =
  case findOnlyCandidate grid of
    Nothing -> Right Stuck
    Just (coord, symbol) ->
      case setCell grid coord symbol of
        Left _ -> Left Contradiction
        Right updatedGrid -> Right (Progress updatedGrid)

----------------------------------------------------------------------
-- Internal Helpers
----------------------------------------------------------------------

findOnlyCandidate :: Grid -> Maybe (Coordinate, Symbol)
findOnlyCandidate grid = maybeHead (mapMaybe (extractOnlyCandidateAt grid) (allCoordinates grid))

extractOnlyCandidateAt :: Grid -> Coordinate -> Maybe (Coordinate, Symbol)
extractOnlyCandidateAt grid coord =
  case cellAt grid coord of
    Just (Empty candidates)
      | S.size candidates == 1 ->
          case S.toList candidates of
            [s] -> Just (coord, s)
            _ -> Nothing
    _ -> Nothing

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x
