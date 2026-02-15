module Sudoku.TestHelpers
  ( requireSymbols
  , requireSymbol
  , requireEmptyGrid
  ) where

import Test.Tasty.HUnit (assertFailure)

import Sudoku.Grid
  ( mkSymbols
  , Symbols
  , mkSymbol
  , Symbol
  , emptyGrid
  , Grid
  )

requireSymbols :: String -> [Char] -> IO Symbols
requireSymbols msg chars =
  case mkSymbols chars of
    Nothing -> assertFailure msg >> pure (error "unreachable")
    Just s  -> pure s

requireSymbol :: String -> Symbols -> Char -> IO Symbol
requireSymbol msg symbols c =
  case mkSymbol symbols c of
    Nothing -> assertFailure msg >> pure (error "unreachable")
    Just s  -> pure s

requireEmptyGrid :: String -> Symbols -> IO Grid
requireEmptyGrid msg allowed =
  case emptyGrid allowed of
    Nothing -> assertFailure msg >> pure (error "unreachable")
    Just g  -> pure g
