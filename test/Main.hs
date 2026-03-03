module Main (main) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import qualified Sudoku.GeometrySpec as GeometrySpec
import qualified Sudoku.GridSpec as GridSpec
import qualified Sudoku.Integration.ValidPuzzlesSpec as ValidPuzzlesSpec
import qualified Sudoku.Math.IntegerRootsSpec as IntegerRootsSpec
import qualified Sudoku.PuzzleBuilderSpec as PuzzleBuilderSpec
import qualified Sudoku.PuzzleParserSpec as PuzzleParserSpec
import qualified Sudoku.Solver.HiddenSingleStrategySpec as HiddenSingleStrategySpec
import qualified Sudoku.Solver.NakedSingleStrategySpec as NakedSingleStrategySpec
import qualified Sudoku.Solver.PuzzleSolverSpec as PuzzleSolverSpec
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
        HiddenSingleStrategySpec.tests,
        NakedSingleStrategySpec.tests,
        IntegerRootsSpec.tests,
        PuzzleBuilderSpec.tests,
        PuzzleParserSpec.tests,
        PuzzleSolverSpec.tests,
        SymbolsSpec.tests,
        ValidPuzzlesSpec.testParseAllValidPuzzles
      ]
