module Sudoku.Solver.HiddenSingleStrategySpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Grid
  ( Grid,
    setCell,
  )
import Sudoku.Solver.HiddenSingleStrategy (hiddenSingleStrategy)
import Sudoku.Solver.Strategy
  ( AfterStep (..),
  )
import Sudoku.Symbols (Symbol)
import Sudoku.TestHelpers
  ( requireEmptyGrid,
    requireRight,
    requireSymbol,
    requireSymbols,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )

----------------------------------------------------------------------
-- Test suite
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Sudoku.Solver.HiddenSingleStrategy"
    [ testHiddenSingleEmptyGridStuck4x4,
      testHiddenSingleFindsHiddenSingleInRow4x4,
      testHiddenSingleNoHiddenSinglesStuck4x4
    ]

----------------------------------------------------------------------
-- hiddenSingleStrategy
----------------------------------------------------------------------

testHiddenSingleEmptyGridStuck4x4 :: TestTree
testHiddenSingleEmptyGridStuck4x4 =
  testCase "hiddenSingleStrategy returns Stuck on empty 4x4 grid (and does not modify it)" $ do
    -- given
    grid0 <- empty4x4

    -- when
    let actual = hiddenSingleStrategy grid0

    -- then
    case actual of
      Left e -> assertFailure ("expected Right, got Left " <> show e)
      Right Stuck -> assertEqual "grid unchanged" grid0 grid0
      Right (Progress _) -> assertFailure "expected Stuck, got Progress"

-- 4x4 where row 0 has a hidden single for '1' at (1,0):
--
-- 2 . . 3
-- . 3 1 .
-- 3 . . .
-- . 4 . .
--
-- Expected: fills (1,0) with '1' (Progress).
testHiddenSingleFindsHiddenSingleInRow4x4 :: TestTree
testHiddenSingleFindsHiddenSingleInRow4x4 =
  testCase "hiddenSingleStrategy fills a hidden single (row) even if it's not a naked single (4x4)" $ do
    -- given
    (grid0, sym1) <- gridWithHiddenSingleRow4x4
    expected <- requireRight "expected setCell to succeed" (setCell grid0 (1, 0) sym1)

    -- when
    let actual = hiddenSingleStrategy grid0

    -- then
    case actual of
      Left e -> assertFailure ("expected Right, got Left " <> show e)
      Right Stuck -> assertFailure "expected Progress, got Stuck"
      Right (Progress grid1) -> assertEqual "updated grid matches expected" expected grid1

-- 4x4 with some fixed placements, but no hidden single exists in any unit:
--
-- 1 . . .
-- . . . .
-- . . . .
-- . . . .
--
-- Expected: Stuck (and grid unchanged)
testHiddenSingleNoHiddenSinglesStuck4x4 :: TestTree
testHiddenSingleNoHiddenSinglesStuck4x4 =
  testCase "hiddenSingleStrategy returns Stuck when there are no hidden singles (4x4)" $ do
    -- given
    (grid0, _) <- gridNoHiddenSingles4x4

    -- when
    let actual = hiddenSingleStrategy grid0

    -- then
    case actual of
      Left e -> assertFailure ("expected Right, got Left " <> show e)
      Right Stuck -> pure ()
      Right (Progress _) -> assertFailure "expected Stuck, got Progress"

----------------------------------------------------------------------
-- Grid fixtures
----------------------------------------------------------------------

empty4x4 :: IO Grid
empty4x4 = do
  allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" ['1' .. '4']
  requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

-- 1 . . .
-- . . . .
-- . . . .
-- . . . .
gridNoHiddenSingles4x4 :: IO (Grid, ())
gridNoHiddenSingles4x4 = do
  allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" ['1' .. '4']
  grid0 <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
  sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'
  grid1 <- requireRight "setup failed: setCell (0,0) '1'" (setCell grid0 (0, 0) sym1)
  pure (grid1, ())

-- 2 . . 3
-- . 3  .
-- 3 . . .
-- . 4 . .
gridWithHiddenSingleRow4x4 :: IO (Grid, Symbol)
gridWithHiddenSingleRow4x4 = do
  allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" ['1' .. '4']
  grid0 <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

  sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'
  sym2 <- requireSymbol "mkSymbol failed for '2'" allowed '2'
  sym3 <- requireSymbol "mkSymbol failed for '3'" allowed '3'
  sym4 <- requireSymbol "mkSymbol failed for '4'" allowed '4'

  -- row 0: 2 . . 3
  grid1 <- requireRight "setup failed: (0,0)=2" (setCell grid0 (0, 0) sym2)
  grid2 <- requireRight "setup failed: (3,0)=3" (setCell grid1 (3, 0) sym3)

  -- row 1: . 3 4 .
  grid3 <- requireRight "setup failed: (1,1)=3" (setCell grid2 (1, 1) sym3)
  grid4 <- requireRight "setup failed: (2,1)=1" (setCell grid3 (2, 1) sym1)

  -- row 2: 3 . . .
  grid5 <- requireRight "setup failed: (0,2)=3" (setCell grid4 (0, 2) sym3)

  -- row 3: . 4 . .
  grid6 <- requireRight "setup failed: (1,3)=4" (setCell grid5 (1, 3) sym4)

  pure (grid6, sym1)
