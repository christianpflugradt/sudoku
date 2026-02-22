module Sudoku.TestHelpers
  ( requireEmptyGrid,
    requireLeft,
    requireRight,
    requireSymbol,
    requireSymbols,
  )
where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Grid
  ( Grid,
    emptyGrid,
  )
import Sudoku.Symbols
  ( Symbol,
    Symbols,
    mkSymbol,
    mkSymbols,
  )
import Test.Tasty.HUnit (assertFailure)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

requireRight :: String -> Either e a -> IO a
requireRight msg = \case
  Left _ -> assertFailure msg >> pure (error "unreachable")
  Right a -> pure a

requireLeft :: String -> Either e a -> IO e
requireLeft msg = \case
  Left e -> pure e
  Right _ -> assertFailure msg >> pure (error "unreachable")

requireSymbols :: String -> [Char] -> IO Symbols
requireSymbols msg chars =
  case mkSymbols chars of
    Nothing -> assertFailure msg >> pure (error "unreachable")
    Just s -> pure s

requireSymbol :: String -> Symbols -> Char -> IO Symbol
requireSymbol msg symbols c =
  case mkSymbol symbols c of
    Nothing -> assertFailure msg >> pure (error "unreachable")
    Just s -> pure s

requireEmptyGrid :: String -> Symbols -> IO Grid
requireEmptyGrid msg allowed =
  case emptyGrid allowed of
    Nothing -> assertFailure msg >> pure (error "unreachable")
    Just g -> pure g
