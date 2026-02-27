module Sudoku.Solver.PuzzleSolverSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Grid (Grid)
import Sudoku.PuzzleBuilder (buildPuzzle)
import Sudoku.Solver.PuzzleSolver
  ( SolveResult (..),
    solveWith,
  )
import Sudoku.Solver.Strategy
  ( AfterStep (..),
    PuzzleError (..),
    Strategy,
  )
import Sudoku.TestHelpers
  ( requireEmptyGrid,
    requireLeft,
    requireRight,
    requireSymbol,
    requireSymbols,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

----------------------------------------------------------------------
-- Test suite
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "PuzzleSolver"
    [ testSolveWithCompleteGrid,
      testSolveWithStuckStrategy,
      testSolveWithProgressStrategy,
      testSolveWithContradiction
    ]

----------------------------------------------------------------------
-- Test cases
----------------------------------------------------------------------

testSolveWithCompleteGrid :: TestTree
testSolveWithCompleteGrid =
  testCase "solveWith returns Solved when grid is complete" $ do
    -- given
    let allowedChars = ['1']
    symbols <- requireSymbols "mkSymbols failed for ['1']" allowedChars
    sym1 <- requireSymbol "mkSymbol failed for '1'" symbols '1'
    grid <- requireRight "expected Right grid" (buildPuzzle symbols [((0, 0), sym1)])

    let expected = Solved grid

    -- when
    actual <- requireRight "expected Right Solved" (solveWith [] grid)

    -- then
    assertEqual "result" expected actual

testSolveWithStuckStrategy :: TestTree
testSolveWithStuckStrategy =
  testCase "solveWith returns Unsolvable when strategies are stuck" $ do
    -- given
    let allowedChars = ['1']
    symbols <- requireSymbols "mkSymbols failed for ['1']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 1x1 symbols" symbols

    let strategies = [stuckStrategy]
    let expected = Unsolvable grid

    -- when
    actual <- requireRight "expected Right Unsolvable" (solveWith strategies grid)

    -- then
    assertEqual "result" expected actual

testSolveWithProgressStrategy :: TestTree
testSolveWithProgressStrategy =
  testCase "solveWith returns Solved when a strategy makes progress" $ do
    -- given
    let allowedChars = ['1']
    symbols <- requireSymbols "mkSymbols failed for ['1']" allowedChars
    sym1 <- requireSymbol "mkSymbol failed for '1'" symbols '1'
    start <- requireEmptyGrid "emptyGrid returned Nothing for 1x1 symbols" symbols
    solved <- requireRight "expected Right grid" (buildPuzzle symbols [((0, 0), sym1)])

    let strategies = [progressTo solved]
    let expected = Solved solved

    -- when
    actual <- requireRight "expected Right Solved" (solveWith strategies start)

    -- then
    assertEqual "result" expected actual

testSolveWithContradiction :: TestTree
testSolveWithContradiction =
  testCase "solveWith returns Left Contradiction when a strategy fails" $ do
    -- given
    let allowedChars = ['1']
    symbols <- requireSymbols "mkSymbols failed for ['1']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 1x1 symbols" symbols

    let strategies = [contradictionStrategy]
    let expected = Contradiction

    -- when
    actual <- requireLeft "expected Left Contradiction" (solveWith strategies grid)

    -- then
    assertEqual "error" expected actual

----------------------------------------------------------------------
-- Test strategies
----------------------------------------------------------------------

stuckStrategy :: Strategy
stuckStrategy _ = Right Stuck

progressTo :: Grid -> Strategy
progressTo solvedGrid _ = Right (Progress solvedGrid)

contradictionStrategy :: Strategy
contradictionStrategy _ = Left Contradiction
