module Sudoku.PuzzleBuilderSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Grid
  ( Cell (..),
    cellAt,
  )
import Sudoku.Placements (PlacementError (..))
import Sudoku.PuzzleBuilder (buildPuzzle)
import Sudoku.TestHelpers
  ( requireLeft,
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
    "PuzzleBuilder"
    [ testBuildPuzzleEmptyPlacements,
      testBuildPuzzleSinglePlacement,
      testBuildPuzzleMultiplePlacements,
      testBuildPuzzlePropagatesOutOfBounds,
      testBuildPuzzlePropagatesAlreadySet,
      testBuildPuzzlePropagatesDuplicateInUnit,
      testBuildPuzzlePropagatesNoCandidates
    ]

----------------------------------------------------------------------
-- Test cases
----------------------------------------------------------------------

testBuildPuzzleEmptyPlacements :: TestTree
testBuildPuzzleEmptyPlacements =
  testCase "buildPuzzle succeeds with empty placements" $ do
    -- given
    let allowedChars = ['1' .. '4']
    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    -- when
    _grid <- requireRight "expected Right grid" (buildPuzzle symbols [])

    -- then
    pure ()

testBuildPuzzleSinglePlacement :: TestTree
testBuildPuzzleSinglePlacement =
  testCase "buildPuzzle sets a single Fixed cell" $ do
    -- given
    let allowedChars = ['1' .. '4']
        coord = (0, 0)
    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    sym1 <- requireSymbol "mkSymbol failed for '1'" symbols '1'

    -- when
    grid <- requireRight "expected Right grid" (buildPuzzle symbols [(coord, sym1)])

    -- then
    assertEqual "cellAt" (Just (Fixed sym1)) (cellAt grid coord)

testBuildPuzzleMultiplePlacements :: TestTree
testBuildPuzzleMultiplePlacements =
  testCase "buildPuzzle applies multiple placements" $ do
    -- given
    let allowedChars = ['1' .. '4']
    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    sym1 <- requireSymbol "mkSymbol failed for '1'" symbols '1'
    sym2 <- requireSymbol "mkSymbol failed for '2'" symbols '2'
    sym3 <- requireSymbol "mkSymbol failed for '3'" symbols '3'

    let placements =
          [ ((0, 0), sym1),
            ((1, 1), sym2),
            ((2, 3), sym3)
          ]

    -- when
    grid <- requireRight "expected Right grid" (buildPuzzle symbols placements)

    -- then
    assertEqual "cellAt (0,0)" (Just (Fixed sym1)) (cellAt grid (0, 0))
    assertEqual "cellAt (1,1)" (Just (Fixed sym2)) (cellAt grid (1, 1))
    assertEqual "cellAt (2,3)" (Just (Fixed sym3)) (cellAt grid (2, 3))

testBuildPuzzlePropagatesOutOfBounds :: TestTree
testBuildPuzzlePropagatesOutOfBounds =
  testCase "buildPuzzle returns Left OutOfBounds if a placement is out of bounds" $ do
    -- given
    let allowedChars = ['1' .. '4']
    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    sym1 <- requireSymbol "mkSymbol failed for '1'" symbols '1'

    let placements =
          [ ((0, 0), sym1),
            ((4, 0), sym1) -- out of bounds for 4x4
          ]

    -- when
    err <- requireLeft "expected Left OutOfBounds" (buildPuzzle symbols placements)

    -- then
    assertEqual "error" OutOfBounds err

testBuildPuzzlePropagatesAlreadySet :: TestTree
testBuildPuzzlePropagatesAlreadySet =
  testCase "buildPuzzle returns Left AlreadySet if the same cell is set twice" $ do
    -- given
    let allowedChars = ['1' .. '4']
        coord = (0, 0)
    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    sym1 <- requireSymbol "mkSymbol failed for '1'" symbols '1'
    sym2 <- requireSymbol "mkSymbol failed for '2'" symbols '2'

    let placements =
          [ (coord, sym1),
            (coord, sym2)
          ]

    -- when
    err <- requireLeft "expected Left AlreadySet" (buildPuzzle symbols placements)

    -- then
    assertEqual "error" AlreadySet err

testBuildPuzzlePropagatesDuplicateInUnit :: TestTree
testBuildPuzzlePropagatesDuplicateInUnit =
  testCase "buildPuzzle returns Left DuplicateInUnit for duplicate symbol in a row" $ do
    -- given
    let allowedChars = ['1' .. '4']
    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    sym1 <- requireSymbol "mkSymbol failed for '1'" symbols '1'

    let placements =
          [ ((0, 0), sym1),
            ((1, 0), sym1)
          ]

    -- when
    err <- requireLeft "expected Left DuplicateInUnit" (buildPuzzle symbols placements)

    -- then
    assertEqual "error" DuplicateInUnit err

testBuildPuzzlePropagatesNoCandidates :: TestTree
testBuildPuzzlePropagatesNoCandidates =
  testCase "buildPuzzle returns Left NoCandidates when a peer loses its last candidate" $ do
    -- given
    let allowedChars = ['1' .. '4']
    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    sym1 <- requireSymbol "mkSymbol failed for '1'" symbols '1'
    sym2 <- requireSymbol "mkSymbol failed for '2'" symbols '2'
    sym3 <- requireSymbol "mkSymbol failed for '3'" symbols '3'
    sym4 <- requireSymbol "mkSymbol failed for '4'" symbols '4'

    let placements =
          [ ((2, 0), sym2),
            ((1, 2), sym3),
            ((0, 1), sym4),
            ((0, 0), sym1) -- removes last candidate from (1,0)
          ]

    -- when
    err <- requireLeft "expected Left NoCandidates" (buildPuzzle symbols placements)

    -- then
    assertEqual "error" NoCandidates err
