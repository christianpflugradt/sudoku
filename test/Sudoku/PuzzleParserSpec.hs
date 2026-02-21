module Sudoku.PuzzleParserSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Data.Foldable (traverse_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)

import Sudoku.PuzzleParser
  ( ParseError(..)
  , parsePuzzle
  )

import Sudoku.Placements (Placements)
import Sudoku.Symbols (Symbols)

import Sudoku.TestHelpers
  ( requireLeft
  , requireRight
  , requireSymbols
  , requireSymbol
  )

----------------------------------------------------------------------
-- Test suite
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "PuzzleParser"
    [ testGroup "parsePuzzleHappyPath"
        [ testParsePuzzleHappyPath4x4Header
        , testParsePuzzleHappyPath4x4HeaderArgs
        , testParsePuzzleHappyPath4x4Args
        , testParsePuzzleHappyPath4x4Inferred
        , testParsePuzzleHappyPath9x9
        , testParsePuzzleHappyPath4x4AdditionalHeaders
        , testParsePuzzleHappyPath4x4GridWhitespaces
        ]
    , testGroup "parsePuzzleParseError"
        [ testParsePuzzleParseErrorInvalidHeader
        , testParsePuzzleParseErrorDuplicateHeader
        , testParsePuzzleParseErrorDuplicateDeclaredSymbols
        , testParsePuzzleParseErrorAmbiguousSymbols
        , testParsePuzzleParseErrorInvalidSymbolsArgs
        , testParsePuzzleParseErrorInvalidSymbolsHeader
        , testParsePuzzleParseErrorSymbolCountMismatchArgs
        , testParsePuzzleParseErrorSymbolCountMismatchHeader
        , testParsePuzzleParseErrorSymbolCountMismatchInferred
        , testParsePuzzleParseErrorMalformedGrid
        , testParsePuzzleParseErrorUnsupportedGridShape
        ]
    ]

----------------------------------------------------------------------
-- Happy Path Tests
----------------------------------------------------------------------

testParsePuzzleHappyPath4x4Header :: TestTree
testParsePuzzleHappyPath4x4Header =
  testCase "parses 4x4 with symbols header (key case-insensitive)" $ do
    -- given
    expectedSymbols <- requireSymbols "mkSymbols failed for '1234'" "1234"
    let expectedCoordsAndChars =
          [ ((0,0), '1')
          , ((3,0), '4')
          , ((0,2), '2')
          , ((3,2), '3')
          , ((0,3), '4')
          , ((2,3), '1')
          , ((3,3), '2')
          ]
    expectedPlacements <- requirePlacements expectedSymbols expectedCoordsAndChars
    let headerVariants =
          [ "symbols: 1234"
          , "Symbols: 1234"
          , "SYMBOLS: 1234"
          ]

    traverse_ (\symbolsHeader -> do
      let input = unlines
            [ symbolsHeader
            , "1..4"
            , "...."
            , "2..3"
            , "4.12"
            ]

      -- when
      (actualSymbols, actualPlacements) <- requireRight ("parsePuzzle returned Left for header: " ++ show symbolsHeader) (parsePuzzle Nothing input)

      -- then
      assertEqual ("symbols for header: " ++ show symbolsHeader) expectedSymbols actualSymbols
      assertEqual ("placements for header: " ++ show symbolsHeader) expectedPlacements actualPlacements
      ) headerVariants

testParsePuzzleHappyPath4x4HeaderArgs :: TestTree
testParsePuzzleHappyPath4x4HeaderArgs =
  testCase "parses 4x4 with symbols header and symbols passed to function" $ do
    -- given
    let input = unlines
          [ "symbols: 1234"
          , "1..4"
          , "...."
          , "2..3"
          , "4.12"
          ]
    expectedSymbols <- requireSymbols "mkSymbols failed for '1234'" "1234"
    let expectedCoordsAndChars =
          [ ((0,0), '1')
          , ((3,0), '4')
          , ((0,2), '2')
          , ((3,2), '3')
          , ((0,3), '4')
          , ((2,3), '1')
          , ((3,3), '2')
          ]
    expectedPlacements <- requirePlacements expectedSymbols expectedCoordsAndChars

    -- when
    (actualSymbols, actualPlacements) <- requireRight "parsePuzzle returned Left" (parsePuzzle (Just expectedSymbols) input)

    -- then
    assertEqual "symbols" expectedSymbols actualSymbols
    assertEqual "placements" expectedPlacements actualPlacements

testParsePuzzleHappyPath4x4Args :: TestTree
testParsePuzzleHappyPath4x4Args =
  testCase "parses 4x4 with no symbols header and symbols passed to function" $ do
    -- given
    let input = unlines
          [ "1..4"
          , "...."
          , "2..3"
          , "4.12"
          ]
    expectedSymbols <- requireSymbols "mkSymbols failed for '1234'" "1234"
    let expectedCoordsAndChars =
          [ ((0,0), '1')
          , ((3,0), '4')
          , ((0,2), '2')
          , ((3,2), '3')
          , ((0,3), '4')
          , ((2,3), '1')
          , ((3,3), '2')
          ]
    expectedPlacements <- requirePlacements expectedSymbols expectedCoordsAndChars

    -- when
    (actualSymbols, actualPlacements) <- requireRight "parsePuzzle returned Left" (parsePuzzle (Just expectedSymbols) input)

    -- then
    assertEqual "symbols" expectedSymbols actualSymbols
    assertEqual "placements" expectedPlacements actualPlacements

testParsePuzzleHappyPath4x4AdditionalHeaders :: TestTree
testParsePuzzleHappyPath4x4AdditionalHeaders =
  testCase "parses 4x4 with extra headers + whitespace lines" $ do
    -- given
    let input = unlines
          [ ""
          , "headers-are-alphanumeric-and-hyphens-are-allowed-also-they-are-CASE-SENSITIVE-and-can-be-very-long: just a comment"
          , "   "
          , "a: a header must have at least one character and not start with a hyphen"
          , "1..4"
          , "...."
          , "2..3"
          , "4.12"
          ]

    expectedSymbols <- requireSymbols "mkSymbols failed for '1234'" "1234"
    let expectedCoordsAndChars =
          [ ((0,0), '1')
          , ((3,0), '4')
          , ((0,2), '2')
          , ((3,2), '3')
          , ((0,3), '4')
          , ((2,3), '1')
          , ((3,3), '2')
          ]
    expectedPlacements <- requirePlacements expectedSymbols expectedCoordsAndChars

    -- when
    (actualSymbols, actualPlacements) <-
      requireRight "parsePuzzle returned Left"
        (parsePuzzle (Just expectedSymbols) input)

    -- then
    assertEqual "symbols" expectedSymbols actualSymbols
    assertEqual "placements" expectedPlacements actualPlacements

testParsePuzzleHappyPath4x4GridWhitespaces :: TestTree
testParsePuzzleHappyPath4x4GridWhitespaces =
  testCase "parses 4x4 with additional whitespaces in grid" $ do
    -- given
    let input = "  1.\t.4 ..\r\n..2\x00A0 . .\r34.12\n\t"
    expectedSymbols <- requireSymbols "mkSymbols failed for '1234'" "1234"
    let expectedCoordsAndChars =
          [ ((0,0), '1')
          , ((3,0), '4')
          , ((0,2), '2')
          , ((3,2), '3')
          , ((0,3), '4')
          , ((2,3), '1')
          , ((3,3), '2')
          ]
    expectedPlacements <- requirePlacements expectedSymbols expectedCoordsAndChars

    -- when
    (actualSymbols, actualPlacements) <- requireRight "parsePuzzle returned Left" (parsePuzzle (Just expectedSymbols) input)

    -- then
    assertEqual "symbols" expectedSymbols actualSymbols
    assertEqual "placements" expectedPlacements actualPlacements

testParsePuzzleHappyPath4x4Inferred :: TestTree
testParsePuzzleHappyPath4x4Inferred =
  testCase "parses 4x4 with symbols inferred from input" $ do
    -- given
    let input = unlines
          [ "1..4"
          , "...."
          , "2..3"
          , "4.12"
          ]
    expectedSymbols <- requireSymbols "mkSymbols failed for '1234'" "1234"
    let expectedCoordsAndChars =
          [ ((0,0), '1')
          , ((3,0), '4')
          , ((0,2), '2')
          , ((3,2), '3')
          , ((0,3), '4')
          , ((2,3), '1')
          , ((3,3), '2')
          ]
    expectedPlacements <- requirePlacements expectedSymbols expectedCoordsAndChars

    -- when
    (actualSymbols, actualPlacements) <- requireRight "parsePuzzle returned Left" (parsePuzzle Nothing input)

    -- then
    assertEqual "symbols" expectedSymbols actualSymbols
    assertEqual "placements" expectedPlacements actualPlacements

testParsePuzzleHappyPath9x9 :: TestTree
testParsePuzzleHappyPath9x9 =
  testCase "parses 9x9 with symbols header" $ do
    -- given
    let input = unlines
          [ "symbols: 123456789"
          , "....65928"
          , "1.5.2.76."
          , "..28....."
          , "53.489..."
          , "64.7..83."
          , "..7.1..49"
          , "49...8157"
          , ".18...3.."
          , "....912.."
          ]
    expectedSymbols <- requireSymbols "mkSymbols failed for '123456789'" "123456789"
    let expectedCoordsAndChars =
          [ ((4,0), '6'), ((5,0), '5'), ((6,0), '9'), ((7,0), '2'), ((8,0), '8')
          , ((0,1), '1'), ((2,1), '5'), ((4,1), '2'), ((6,1), '7'), ((7,1), '6')
          , ((2,2), '2'), ((3,2), '8')
          , ((0,3), '5'), ((1,3), '3'), ((3,3), '4'), ((4,3), '8'), ((5,3), '9')
          , ((0,4), '6'), ((1,4), '4'), ((3,4), '7'), ((6,4), '8'), ((7,4), '3')
          , ((2,5), '7'), ((4,5), '1'), ((7,5), '4'), ((8,5), '9')
          , ((0,6), '4'), ((1,6), '9'), ((5,6), '8'), ((6,6), '1'), ((7,6), '5'), ((8,6), '7')
          , ((1,7), '1'), ((2,7), '8'), ((6,7), '3')
          , ((4,8), '9'), ((5,8), '1'), ((6,8), '2')
          ]
    expectedPlacements <- requirePlacements expectedSymbols expectedCoordsAndChars

    -- when
    (actualSymbols, actualPlacements) <-
      requireRight "parsePuzzle returned Left" (parsePuzzle Nothing input)

    -- then
    assertEqual "symbols" expectedSymbols actualSymbols
    assertEqual "placements" expectedPlacements actualPlacements

----------------------------------------------------------------------
-- ParseError Tests
----------------------------------------------------------------------

testParsePuzzleParseErrorInvalidHeader :: TestTree
testParsePuzzleParseErrorInvalidHeader =
  testCase "rejects invalid header lines" $ do
    let invalidHeaders =
          [ "-begins-with-hyphen: some value"
          , "has_underscore: some value"
          , "has.dot: some value"
          , "has/slash: some value"
          , "header-without-value: "
          , "separator-without-space:some value"
          ]

    -- given
    traverse_ (\header -> do
      let input = unlines
            [ header
            , "1..4"
            , "...."
            , "2..3"
            , "4.12"
            ]

      -- when
      actual <- requireLeft ("expected Left for header: " ++ show header) (parsePuzzle Nothing input)

      -- then
      assertEqual ("error for header: " ++ show header) InvalidHeader actual
      ) invalidHeaders

testParsePuzzleParseErrorDuplicateHeader :: TestTree
testParsePuzzleParseErrorDuplicateHeader =
  testCase "rejects duplicate header lines" $ do
    let duplicateHeaders =
          [ ("duplicate-header: some value", "duplicate-header: some other value")
          , ("another-duplicate: this is lower case", "AnotheR-DuplicatE: this is only a duplicate after case normalization")
          ]

    -- given
    traverse_ (\header -> do
      let input = unlines
            [ fst header
            , snd header
            , "1..4"
            , "...."
            , "2..3"
            , "4.12"
            ]

      -- when
      actual <- requireLeft ("expected Left for header: " ++ show header) (parsePuzzle Nothing input)

      -- then
      assertEqual ("error for header: " ++ show header) DuplicateHeader actual
      ) duplicateHeaders

testParsePuzzleParseErrorDuplicateDeclaredSymbols :: TestTree
testParsePuzzleParseErrorDuplicateDeclaredSymbols =
  testCase "rejects duplicate symbols in header" $ do
    -- given
    let input = unlines
          [ "symbols: 1124"
          , "1..4"
          , "...."
          , "2..3"
          , "4.12"
          ]

    -- when
    actual <- requireLeft "expected Left for 'symbols: 1124'" (parsePuzzle Nothing input)

    -- then
    assertEqual "error for 'symbols: 1124'" DuplicateDeclaredSymbols actual

testParsePuzzleParseErrorAmbiguousSymbols:: TestTree
testParsePuzzleParseErrorAmbiguousSymbols =
  testCase "rejects ambiguous symbols (args / header mismatch)" $ do
    -- given
    let input = unlines
          [ "symbols: 1234"
          , "1..4"
          , "...."
          , "2..3"
          , "4.12"
          ]
    argsSymbols <- requireSymbols "mkSymbols failed for '6789'" "6789"

    -- when
    actual <- requireLeft "expected Left for symbols mismatch" (parsePuzzle (Just argsSymbols) input)

    -- then
    assertEqual "error for symbols mismatch" AmbiguousSymbols actual

testParsePuzzleParseErrorInvalidSymbolsArgs :: TestTree
testParsePuzzleParseErrorInvalidSymbolsArgs =
  testCase "rejects invalid symbols in args" $ do
    let input = unlines
          [ "1..4"
          , "...."
          , "2..3"
          , "4.12"
          ]
        invalidArgsSymbols =
          [ "123:"
          , "123."
          ]

    traverse_ (\args -> do
      -- given
      argsSymbols <- requireSymbols ("mkSymbols failed for " ++ show args) args

      -- when
      actual <- requireLeft ("expected Left for args symbols: " ++ show args)
                            (parsePuzzle (Just argsSymbols) input)

      -- then
      assertEqual ("error for args symbols: " ++ show args) InvalidSymbols actual
      ) invalidArgsSymbols

testParsePuzzleParseErrorInvalidSymbolsHeader :: TestTree
testParsePuzzleParseErrorInvalidSymbolsHeader =
  testCase "rejects invalid symbols in header" $ do
    let invalidHeaderSymbols =
          [ "123:"
          , "123."
          ]

    traverse_ (\header -> do
      -- given
      let input = unlines
            [ "symbols: " ++ header
            , "1..4"
            , "...."
            , "2..3"
            , "4.12"
            ]

      -- when
      actual <- requireLeft ("expected Left for header symbols: " ++ show header)
                            (parsePuzzle Nothing input)

      -- then
      assertEqual ("error for header symbols: " ++ show header) InvalidSymbols actual
      ) invalidHeaderSymbols

testParsePuzzleParseErrorSymbolCountMismatchArgs :: TestTree
testParsePuzzleParseErrorSymbolCountMismatchArgs =
  testCase "rejects symbols in args not matching grid size" $ do
    let input = unlines
          [ "1..4"
          , "...."
          , "2..3"
          , "4.12"
          ]
        invalidArgsSymbols =
          [ "123"
          , "12345"
          ]

    traverse_ (\args -> do
      -- given
      argsSymbols <- requireSymbols ("mkSymbols failed for " ++ show args) args

      -- when
      actual <- requireLeft ("expected Left for args symbols: " ++ show args)
                            (parsePuzzle (Just argsSymbols) input)

      -- then
      assertEqual ("error for args symbols: " ++ show args) SymbolCountMismatch actual
      ) invalidArgsSymbols

testParsePuzzleParseErrorSymbolCountMismatchHeader :: TestTree
testParsePuzzleParseErrorSymbolCountMismatchHeader =
  testCase "rejects symbols in header not matching grid size" $ do
    let invalidHeaderSymbols =
          [ "123"
          , "12345"
          ]

    traverse_ (\header -> do
      -- given
      let input = unlines
            [ "symbols: " ++ header
            , "1..4"
            , "...."
            , "2..3"
            , "4.12"
            ]

      -- when
      actual <- requireLeft ("expected Left for header symbols: " ++ show header)
                            (parsePuzzle Nothing input)

      -- then
      assertEqual ("error for header symbols: " ++ show header) SymbolCountMismatch actual
      ) invalidHeaderSymbols

testParsePuzzleParseErrorSymbolCountMismatchInferred :: TestTree
testParsePuzzleParseErrorSymbolCountMismatchInferred =
  testCase "rejects inferred symbols not matching grid size" $ do
    let inputs =
          [ unlines
              [ ".1.3"
              , "32.."
              , "..32"
              , "2.1."
              ]
          , unlines
              [ ".1.3"
              , "5..."
              , "..42"
              , "2.1."
              ]
          ]

    traverse_ (\input -> do
      -- when
      actual <- requireLeft "expected SymbolCountMismatch (inferred symbols)"  (parsePuzzle Nothing input)

      -- then
      assertEqual "error should be SymbolCountMismatch" SymbolCountMismatch actual
      ) inputs

testParsePuzzleParseErrorMalformedGrid :: TestTree
testParsePuzzleParseErrorMalformedGrid =
  testCase "rejects malformed grids (non-square payload length)" $ do
    let malformedInputs =
          [ unlines
              [ "comment: empty grid is considered Malformed"
              ]
          , unlines
              [ "comment: 4x4 grid has 17 cells instead of 16"
              , ".1..4"
              , "...."
              , "2..3"
              , "4.12"
              ]
          , unlines
              [ "comment: 9x9 grid has 80 cells instead of 81"
              , "....65928"
              , "1.5.2.76."
              , "..28....."
              , "53.489..."
              , "64.7..83."
              , "..7.1..49"
              , "49...8157"
              , ".18...3.."
              , "....912."
              ]
          , unlines
              [ "comment: 4x4 grid has invalid character ':' but correct payload size"
              , "symbols: 1234"
              , "1..4"
              , "...."
              , "2.:."
              , "4.12"
              ]
          ]

    traverse_ (\input -> do
      actual <- requireLeft "expected MalformedGrid" (parsePuzzle Nothing input)
      assertEqual "error should be MalformedGrid" MalformedGrid actual
      ) malformedInputs

testParsePuzzleParseErrorUnsupportedGridShape :: TestTree
testParsePuzzleParseErrorUnsupportedGridShape =
  testCase "rejects square grids with unsupported box shape (e.g. 6x6)" $ do
    let unsupportedInputs =
          [ unlines
              [ "comment: 5x5 grid is no valid sudoku and thus unsupported"
              , "symbols: 12345"
              , "....."
              , "....."
              , "....."
              , "....."
              , "....."
              ]
          , unlines
              [ "comment: 6x6 grid is valid sudoku but currently not supported (non-square boxes)"
              , "symbols: 123456"
              , "......"
              , "......"
              , "......"
              , "......"
              , "......"
              , "......"
              ]
          ]

    traverse_ (\input -> do
      actual <- requireLeft "expected UnsupportedGridShape" (parsePuzzle Nothing input)
      assertEqual "error should be UnsupportedGridShape" UnsupportedGridShape actual
      ) unsupportedInputs

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

requirePlacements :: Symbols -> [((Int, Int), Char)] -> IO Placements
requirePlacements symbols =
  traverse (\(coord, ch) -> do
    sym <- requireSymbol ("mkSymbol failed for " ++ show ch) symbols ch
    pure (coord, sym)
  )
