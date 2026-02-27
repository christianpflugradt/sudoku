module Sudoku.PuzzleSolverSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.PuzzleBuilder (buildPuzzle)
import Sudoku.PuzzleSolver (SolveResult (..), solve)
import Sudoku.TestHelpers
  ( requireEmptyGrid,
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
    [ testSolveCompleteGrid,
      testSolveIncompleteGrid
    ]

----------------------------------------------------------------------
-- Test cases
----------------------------------------------------------------------

testSolveCompleteGrid :: TestTree
testSolveCompleteGrid =
  testCase "solve returns Solved when grid is complete" $ do
    -- given
    let allowedChars = ['1']
    symbols <- requireSymbols "mkSymbols failed for ['1']" allowedChars
    sym1 <- requireSymbol "mkSymbol failed for '1'" symbols '1'
    grid <- requireRight "expected Right grid" (buildPuzzle symbols [((0, 0), sym1)])

    -- when
    result <- requireRight "expected Right Solved" (solve grid)

    -- then
    assertEqual "result" (Solved grid) result

testSolveIncompleteGrid :: TestTree
testSolveIncompleteGrid =
  testCase "solve returns Unsolvable when no strategy makes progress" $ do
    -- given
    let allowedChars = ['1']
    symbols <- requireSymbols "mkSymbols failed for ['1']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 1x1 symbols" symbols

    -- when
    result <- requireRight "expected Right Unsolvable" (solve grid)

    -- then
    assertEqual "result" (Unsolvable grid) result
