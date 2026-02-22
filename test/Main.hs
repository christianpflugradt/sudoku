module Main (main) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import qualified Sudoku.GeometrySpec as GeometrySpec
import qualified Sudoku.GridSpec as GridSpec
import qualified Sudoku.Integration.ValidPuzzlesSpec as ValidPuzzlesSpec
import qualified Sudoku.PuzzleParserSpec as PuzzleParserSpec
import qualified Sudoku.SymbolsSpec as SymbolsSpec
import Test.Tasty (defaultMain, testGroup)

----------------------------------------------------------------------
-- Main
----------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "sudoku"
      [ GeometrySpec.tests,
        GridSpec.tests,
        PuzzleParserSpec.tests,
        SymbolsSpec.tests,
        ValidPuzzlesSpec.testParseAllValidPuzzles
      ]
