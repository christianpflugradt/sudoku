module Sudoku.PuzzleParser
  ( ParseError(..)
  , parsePuzzle
  ) where

import Sudoku.Symbols
  ( mkSymbol
  , mkSymbols
  , Symbols
  )

import Sudoku.Placements
  ( Placements
  )

----------------------------------------------------------------------
-- * Public Types
----------------------------------------------------------------------

data ParseError
  = UnexpectedContent
  | InvalidHeader
  | MalformedGrid
  | UnsupportedGridShape
  | EmptyDeclaredSymbols
  | DuplicateDeclaredSymbols
  | InvalidDeclaredSymbols
  | AmbiguousSymbols
  | InvalidCellSymbol
  | InvalidGiven
  deriving (Eq, Show)

----------------------------------------------------------------------
-- * Public API
----------------------------------------------------------------------

-- Stub implementation: ignores input and returns a fixed dummy result.
parsePuzzle :: Maybe Symbols -> String -> Either ParseError (Symbols, Placements)
parsePuzzle _ _ = case parsed of
    Nothing -> Left InvalidCellSymbol
    Just p  -> Right (symbols, p)
  where
    symbols = case mkSymbols ['1'..'4'] of
      Nothing -> error "unreachable: mkSymbols ['1'..'4'] must succeed"
      Just s  -> s
    parsed = traverse (\(coord, ch) -> (coord,) <$> mkSymbol symbols ch) coordsAndChars
    coordsAndChars =
      [ ((3,0), '4')
      , ((0,2), '2')
      , ((3,2), '3')
      , ((0,3), '4')
      , ((2,3), '1')
      , ((3,3), '2')
      ]
