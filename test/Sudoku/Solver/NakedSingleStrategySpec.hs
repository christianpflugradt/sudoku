module Sudoku.Solver.NakedSingleStrategySpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Grid
  ( Grid,
    setCell,
  )
import Sudoku.Solver.NakedSingleStrategy (nakedSingleStrategy)
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
    "Sudoku.Solver.NakedSingleStrategy"
    [ testNakedSingleEmptyGridStuck4x4,
      testNakedSingleMultipleSinglesChoosesFirst4x4,
      testNakedSingleNoSinglesStuck4x4
    ]

----------------------------------------------------------------------
-- nakedSingleStrategy
----------------------------------------------------------------------

testNakedSingleEmptyGridStuck4x4 :: TestTree
testNakedSingleEmptyGridStuck4x4 =
  testCase "nakedSingleStrategy returns Stuck on empty 4x4 grid (and does not modify it)" $ do
    -- given
    grid0 <- empty4x4

    -- when
    let actual = nakedSingleStrategy grid0

    -- then
    case actual of
      Left e -> assertFailure ("expected Right, got Left " <> show e)
      Right Stuck -> assertEqual "grid unchanged" grid0 grid0
      Right (Progress _) -> assertFailure "expected Stuck, got Progress"

-- 4x4 with multiple naked singles; first by allCoordinates order is (0,1) -> '1':
--
-- . . . .
-- . 2 4 .
-- 3 1 . .
-- . 4 . .
--
-- Expected: fills (0,1) with '1' (Progress), not the other single.
testNakedSingleMultipleSinglesChoosesFirst4x4 :: TestTree
testNakedSingleMultipleSinglesChoosesFirst4x4 =
  testCase "nakedSingleStrategy fills the first naked single by allCoordinates order (4x4)" $ do
    -- given
    (grid0, sym1) <- gridWithMultipleSingles4x4
    expected <- requireRight "expected setCell to succeed" (setCell grid0 (0, 1) sym1)

    -- when
    let actual = nakedSingleStrategy grid0

    -- then
    case actual of
      Left e -> assertFailure ("expected Right, got Left " <> show e)
      Right Stuck -> assertFailure "expected Progress, got Stuck"
      Right (Progress grid1) -> assertEqual "updated grid matches expected" expected grid1

-- 4x4 with some fixed placements, but no cell has exactly one candidate:
--
-- 1 . . .
-- . . . .
-- . . . .
-- . . . .
--
-- Expected: Stuck (and grid unchanged)
testNakedSingleNoSinglesStuck4x4 :: TestTree
testNakedSingleNoSinglesStuck4x4 =
  testCase "nakedSingleStrategy returns Stuck when there are no naked singles (4x4)" $ do
    -- given
    (grid0, _) <- gridNoSingles4x4

    -- when
    let actual = nakedSingleStrategy grid0

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
gridNoSingles4x4 :: IO (Grid, ())
gridNoSingles4x4 = do
  allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" ['1' .. '4']
  grid0 <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
  sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'
  grid1 <- requireRight "setup failed: setCell (0,0) '1'" (setCell grid0 (0, 0) sym1)
  pure (grid1, ())

-- . . . .
-- . 2 4 .
-- 3 1 . .
-- . 4 . .
gridWithMultipleSingles4x4 :: IO (Grid, Symbol)
gridWithMultipleSingles4x4 = do
  allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" ['1' .. '4']
  grid0 <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

  sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'
  sym2 <- requireSymbol "mkSymbol failed for '2'" allowed '2'
  sym3 <- requireSymbol "mkSymbol failed for '3'" allowed '3'
  sym4 <- requireSymbol "mkSymbol failed for '4'" allowed '4'

  grid1 <- requireRight "setup failed: (1,1)=2" (setCell grid0 (1, 1) sym2)
  grid2 <- requireRight "setup failed: (2,1)=4" (setCell grid1 (2, 1) sym4)
  grid3 <- requireRight "setup failed: (0,2)=3" (setCell grid2 (0, 2) sym3)
  grid4 <- requireRight "setup failed: (1,2)=1" (setCell grid3 (1, 2) sym1)
  grid5 <- requireRight "setup failed: (1,3)=4" (setCell grid4 (1, 3) sym4)

  pure (grid5, sym1)
