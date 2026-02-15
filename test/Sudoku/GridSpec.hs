module Sudoku.GridSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure)

import Sudoku.Grid
  ( mkSymbols
  , Symbols
  , emptyGrid
  )

----------------------------------------------------------------------
-- Test suite
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Grid"
    [ testGroup "mkSymbols"
        [ testMkSymbolsEmpty
        , testMkSymbolsUnique4
        , testMkSymbolsSingle
        , testMkSymbolsDuplicateAdjacent
        , testMkSymbolsDuplicateNonAdjacent
        ]
    , testGroup "emptyGrid"
        [ testEmptyGrid1x1Just
        , testEmptyGrid4x4Just
        , testEmptyGrid9x9Just
        , testEmptyGrid16x16Just
        , testEmptyGrid2SymbolsNothing
        , testEmptyGrid3SymbolsNothing
        , testEmptyGrid6SymbolsNothing
        ]
    ]

----------------------------------------------------------------------
-- mkSymbols
----------------------------------------------------------------------

testMkSymbolsEmpty :: TestTree
testMkSymbolsEmpty =
  testCase "empty list -> Nothing" $ do
    -- given
    let input = []

    -- when
    let actual = mkSymbols input

    -- then
    assertEqual "mkSymbols" Nothing actual

testMkSymbolsUnique4 :: TestTree
testMkSymbolsUnique4 =
  testCase "unique list -> Just" $ do
    -- given
    let input = ['1'..'4']

    -- when
    let actual = mkSymbols input

    -- then
    case actual of
      Just _  -> pure ()
      Nothing -> assertFailure "expected Just value"

testMkSymbolsSingle :: TestTree
testMkSymbolsSingle =
  testCase "single symbol -> Just" $ do
    -- given
    let input = ['1']

    -- when
    let actual = mkSymbols input

    -- then
    case actual of
      Just _  -> pure ()
      Nothing -> assertFailure "expected Just value"

testMkSymbolsDuplicateAdjacent :: TestTree
testMkSymbolsDuplicateAdjacent =
  testCase "adjacent duplicate -> Nothing" $ do
    -- given
    let input = ['1','1','2','3']

    -- when
    let actual = mkSymbols input

    -- then
    assertEqual "mkSymbols" Nothing actual

testMkSymbolsDuplicateNonAdjacent :: TestTree
testMkSymbolsDuplicateNonAdjacent =
  testCase "non-adjacent duplicate -> Nothing" $ do
    -- given
    let input = ['1','2','3','1']

    -- when
    let actual = mkSymbols input

    -- then
    assertEqual "mkSymbols" Nothing actual

----------------------------------------------------------------------
-- emptyGrid
----------------------------------------------------------------------

testEmptyGrid1x1Just :: TestTree
testEmptyGrid1x1Just =
  testCase "emptyGrid ['1'] returns Just (1x1 grid)" $ do
    -- given
    let allowedChars = ['1']

    allowed <- requireSymbols "mkSymbols failed for ['1']" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    case actual of
      Just _  -> pure ()
      Nothing -> assertFailure "expected Just grid"

testEmptyGrid4x4Just :: TestTree
testEmptyGrid4x4Just =
  testCase "emptyGrid ['1'..'4'] returns Just (4x4 grid)" $ do
    -- given
    let allowedChars = ['1'..'4']

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    case actual of
      Just _  -> pure ()
      Nothing -> assertFailure "expected Just grid"

testEmptyGrid9x9Just :: TestTree
testEmptyGrid9x9Just =
  testCase "emptyGrid ['1'..'9'] returns Just (9x9 grid)" $ do
    -- given
    let allowedChars = ['1'..'9']

    allowed <- requireSymbols "mkSymbols failed for ['1'..'9']" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    case actual of
      Just _  -> pure ()
      Nothing -> assertFailure "expected Just grid"

testEmptyGrid16x16Just :: TestTree
testEmptyGrid16x16Just =
  testCase "emptyGrid 16x16 alphabet returns Just (16x16 grid)" $ do
    -- given
    let allowedChars = ['1'..'9'] ++ ['A'..'G']

    allowed <- requireSymbols "mkSymbols failed for 16x16 alphabet" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    case actual of
      Just _  -> pure ()
      Nothing -> assertFailure "expected Just grid"

testEmptyGrid2SymbolsNothing :: TestTree
testEmptyGrid2SymbolsNothing =
  testCase "emptyGrid with 2 symbols returns Nothing" $ do
    -- given
    let allowedChars = ['1','2']

    allowed <- requireSymbols "mkSymbols failed for ['1','2']" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    assertEqual "emptyGrid" Nothing actual

testEmptyGrid3SymbolsNothing :: TestTree
testEmptyGrid3SymbolsNothing =
  testCase "emptyGrid with 3 symbols returns Nothing" $ do
    -- given
    let allowedChars = ['1','2','3']

    allowed <- requireSymbols "mkSymbols failed for ['1','2','3']" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    assertEqual "emptyGrid" Nothing actual

testEmptyGrid6SymbolsNothing :: TestTree
testEmptyGrid6SymbolsNothing =
  testCase "emptyGrid with 6 symbols returns Nothing" $ do
    -- given
    let allowedChars = ['1','2','3','4','5','6']

    allowed <- requireSymbols "mkSymbols failed for 6 symbols" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    assertEqual "emptyGrid" Nothing actual

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

requireSymbols :: String -> [Char] -> IO Symbols
requireSymbols msg chars =
  case mkSymbols chars of
    Nothing -> assertFailure msg >> pure (error "unreachable")
    Just s  -> pure s
