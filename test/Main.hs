module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Sudoku.GridSpec as GridSpec
import qualified Sudoku.SymbolsSpec as SymbolsSpec
import qualified Sudoku.UnitsSpec as UnitsSpec

main :: IO ()
main =
  defaultMain $
    testGroup "sudoku"
      [ GridSpec.tests
      , SymbolsSpec.tests
      , UnitsSpec.tests
      ]
