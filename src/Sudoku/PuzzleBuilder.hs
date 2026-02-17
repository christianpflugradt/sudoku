module Sudoku.PuzzleBuilder
  ( buildPuzzle
  ) where

import Sudoku.Grid
  ( Grid
  , emptyGrid
  , setCell
  , Cell(..)
  )

import Sudoku.Symbols
  ( mkSymbol
  , mkSymbols
  , Symbols
  )

import Sudoku.Placements
  ( Placements
  , PlacementError(..)
  )

----------------------------------------------------------------------
-- * Public API
----------------------------------------------------------------------

-- Stub implementation: ignores input and returns a fixed dummy grid.
buildPuzzle :: Symbols -> Placements -> Either PlacementError Grid
buildPuzzle _ _ =
  case emptyGrid symbols of
    Nothing -> error "unreachable: emptyGrid ['1'..'4'] must succeed"
    Just grid ->
      case applied grid of
        Nothing    -> Left PlacementError
        Just grid' -> Right grid'
  where
    symbols =
      case mkSymbols ['1'..'4'] of
        Nothing -> error "unreachable: mkSymbols ['1'..'4'] must succeed"
        Just s  -> s
    givens =
      [ ((3,0), '4')
      , ((0,2), '2')
      , ((3,2), '3')
      , ((0,3), '4')
      , ((2,3), '1')
      , ((3,3), '2')
      ]
    applied :: Grid -> Maybe Grid
    applied grid =
      case traverse
        (\(coord, ch) ->
           case mkSymbol symbols ch of
             Nothing     -> Nothing
             Just symbol -> Just (coord, Fixed symbol)
        )
        givens
      of
        Nothing -> Nothing
        Just cellsToSet ->
          Just (foldl (\g (c, cell) -> setCell g c cell) grid cellsToSet)
