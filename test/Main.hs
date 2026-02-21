module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Sudoku.GridSpec as GridSpec
import qualified Sudoku.PuzzleParserSpec as PuzzleParserSpec
import qualified Sudoku.SymbolsSpec as SymbolsSpec
import qualified Sudoku.Integration.ValidPuzzlesSpec as ValidPuzzlesSpec

main :: IO ()
main =
  defaultMain $
    testGroup "sudoku"
      [ GridSpec.tests
      , PuzzleParserSpec.tests
      , SymbolsSpec.tests
      , ValidPuzzlesSpec.testParseAllValidPuzzles
      ]
