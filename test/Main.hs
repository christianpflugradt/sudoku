module Main where

import qualified Sudoku.GridSpec as GridSpec
import qualified Sudoku.Integration.ValidPuzzlesSpec as ValidPuzzlesSpec
import qualified Sudoku.PuzzleParserSpec as PuzzleParserSpec
import qualified Sudoku.SymbolsSpec as SymbolsSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "sudoku"
      [ GridSpec.tests,
        PuzzleParserSpec.tests,
        SymbolsSpec.tests,
        ValidPuzzlesSpec.testParseAllValidPuzzles
      ]
