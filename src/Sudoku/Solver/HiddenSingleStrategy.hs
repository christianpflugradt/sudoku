module Sudoku.Solver.HiddenSingleStrategy (hiddenSingleStrategy) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as S
import Sudoku.Geometry
  ( Coordinate,
    SideLength (..),
    Unit,
    allUnits,
  )
import Sudoku.Grid
  ( Cell (..),
    Grid,
    allowedSymbols,
    cellAt,
    setCell,
    sideLength,
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

hiddenSingleStrategy :: Strategy
hiddenSingleStrategy grid =
  case findOnlyCell grid of
    Nothing -> Right Stuck
    Just (coord, symbol) ->
      case setCell grid coord symbol of
        Left _ -> Left Contradiction
        Right updatedGrid -> Right (Progress updatedGrid)

----------------------------------------------------------------------
-- Internal Helpers
----------------------------------------------------------------------

type Placement = (Coordinate, Symbol)

data CountResult = Exact Placement | Ambiguous Int

findOnlyCell :: Grid -> Maybe Placement
findOnlyCell grid =
  listToMaybe . mapMaybe (exactMatch . uncurry (countCandidateCells grid)) $
    [(unit, symbol) | symbol <- allowedSymbols grid, unit <- allUnits (SideLength (sideLength grid))]

countCandidateCells :: Grid -> Unit -> Symbol -> CountResult
countCandidateCells grid unit symbol = case coords of
  [coord] -> Exact (coord, symbol)
  _ -> Ambiguous (length coords)
  where
    coords = [coord | coord <- unit, Just cell <- [cellAt grid coord], hasCandidate cell symbol]

hasCandidate :: Cell -> Symbol -> Bool
hasCandidate (Empty candidates) symbol = S.member symbol candidates
hasCandidate _ _ = False

exactMatch :: CountResult -> Maybe Placement
exactMatch (Exact p) = Just p
exactMatch _ = Nothing
