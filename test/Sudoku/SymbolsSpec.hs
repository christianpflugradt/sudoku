module Sudoku.SymbolsSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Symbols
  ( mkSymbol,
    mkSymbols,
  )
import Sudoku.TestHelpers
  ( requireSymbols,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

----------------------------------------------------------------------
-- Test suite
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Grid"
    [ testGroup
        "mkSymbols"
        [ testMkSymbolsEmpty,
          testMkSymbolsUnique4,
          testMkSymbolsSingle,
          testMkSymbolsDuplicateAdjacent,
          testMkSymbolsDuplicateNonAdjacent
        ],
      testGroup
        "mkSymbol"
        [ testMkSymbolAllowed,
          testMkSymbolDisallowed
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
    let input = ['1' .. '4']

    -- when
    let actual = mkSymbols input

    -- then
    case actual of
      Just _ -> pure ()
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
      Just _ -> pure ()
      Nothing -> assertFailure "expected Just value"

testMkSymbolsDuplicateAdjacent :: TestTree
testMkSymbolsDuplicateAdjacent =
  testCase "adjacent duplicate -> Nothing" $ do
    -- given
    let input = ['1', '1', '2', '3']

    -- when
    let actual = mkSymbols input

    -- then
    assertEqual "mkSymbols" Nothing actual

testMkSymbolsDuplicateNonAdjacent :: TestTree
testMkSymbolsDuplicateNonAdjacent =
  testCase "non-adjacent duplicate -> Nothing" $ do
    -- given
    let input = ['1', '2', '3', '1']

    -- when
    let actual = mkSymbols input

    -- then
    assertEqual "mkSymbols" Nothing actual

----------------------------------------------------------------------
-- mkSymbol
----------------------------------------------------------------------

testMkSymbolAllowed :: TestTree
testMkSymbolAllowed =
  testCase "allowed char -> Just" $ do
    -- given
    let allowedChars = ['1' .. '4']
        input = '2'

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    -- when
    let actual = mkSymbol allowed input

    -- then
    case actual of
      Just _ -> pure ()
      Nothing -> assertFailure "expected Just value"

testMkSymbolDisallowed :: TestTree
testMkSymbolDisallowed =
  testCase "disallowed char -> Nothing" $ do
    -- given
    let allowedChars = ['1' .. '4']
        input = '9'

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    -- when
    let actual = mkSymbol allowed input

    -- then
    assertEqual "mkSymbol" Nothing actual
