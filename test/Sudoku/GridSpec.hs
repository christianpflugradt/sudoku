module Sudoku.GridSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure)

import qualified Data.Set as S
import Data.Maybe (mapMaybe)

import Sudoku.Grid
  ( mkSymbols
  , mkSymbol
  , emptyGrid
  , Cell(..)
  , sideLength
  , boundsOf
  , cellAt
  , setCell
  , allCoordinates
  )

import Sudoku.TestHelpers
  ( requireSymbol
  , requireSymbols
  , requireEmptyGrid
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
    , testGroup "mkSymbol"
        [ testMkSymbolAllowed
        , testMkSymbolDisallowed
        ]
    , testGroup "emptyGrid"
        [ testEmptyGridBounds4x4
        , testEmptyGridCandidates4x4
        , testEmptyGridBounds9x9
        , testEmptyGridCandidates9x9
        , testEmptyGridBounds16x16
        , testEmptyGridCandidates16x16
        , testEmptyGrid1x1Just
        , testEmptyGrid2SymbolsNothing
        , testEmptyGrid3SymbolsNothing
        , testEmptyGrid6SymbolsNothing
        ]
    , testGroup "sideLength"
        [ testSideLength4x4
        , testSideLength9x9
        , testSideLength16x16
        ]
    , testGroup "setCell"
        [ testSetCellUpdatesOnlyTarget
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
-- mkSymbol
----------------------------------------------------------------------

testMkSymbolAllowed :: TestTree
testMkSymbolAllowed =
  testCase "allowed char -> Just" $ do
    -- given
    let allowedChars = ['1'..'4']
        input        = '2'

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    -- when
    let actual = mkSymbol allowed input

    -- then
    case actual of
      Just _  -> pure ()
      Nothing -> assertFailure "expected Just value"

testMkSymbolDisallowed :: TestTree
testMkSymbolDisallowed =
  testCase "disallowed char -> Nothing" $ do
    -- given
    let allowedChars = ['1'..'4']
        input        = '9'

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    -- when
    let actual = mkSymbol allowed input

    -- then
    assertEqual "mkSymbol" Nothing actual

----------------------------------------------------------------------
-- emptyGrid
----------------------------------------------------------------------

testEmptyGridBounds4x4 :: TestTree
testEmptyGridBounds4x4 =
  testCase "emptyGrid ['1'..'4'] has 4x4 bounds" $ do
    -- given
    let allowedChars   = ['1'..'4']
        expectedBounds = ((0,0),(3,3))

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- then
    assertEqual "bounds" expectedBounds (boundsOf actual)

testEmptyGridCandidates4x4 :: TestTree
testEmptyGridCandidates4x4 =
  testCase "emptyGrid ['1'..'4'] has full candidate set in every cell" $ do
    -- given
    let allowedChars = ['1'..'4']

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    let expected = S.fromList (mapMaybe (mkSymbol allowed) allowedChars)

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- then
    mapM_
      (\coord ->
         case cellAt actual coord of
           Nothing ->
             assertFailure ("cellAt returned Nothing for in-bounds coord " ++ show coord)
           Just (Empty c) ->
             assertEqual "candidates" expected c
           Just (Fixed _) ->
             assertFailure "expected Empty cell"
      )
      (allCoordinates actual)

testEmptyGridBounds9x9 :: TestTree
testEmptyGridBounds9x9 =
  testCase "emptyGrid ['1'..'9'] has 9x9 bounds" $ do
    -- given
    let allowedChars   = ['1'..'9']
        expectedBounds = ((0,0),(8,8))

    allowed <- requireSymbols "mkSymbols failed for ['1'..'9']" allowedChars

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 9x9 symbols" allowed

    -- then
    assertEqual "bounds" expectedBounds (boundsOf actual)

testEmptyGridCandidates9x9 :: TestTree
testEmptyGridCandidates9x9 =
  testCase "emptyGrid ['1'..'9'] has full candidate set in every cell" $ do
    -- given
    let allowedChars = ['1'..'9']

    allowed <- requireSymbols "mkSymbols failed for ['1'..'9']" allowedChars

    let expected = S.fromList (mapMaybe (mkSymbol allowed) allowedChars)

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 9x9 symbols" allowed

    -- then
    mapM_
      (\coord ->
         case cellAt actual coord of
           Nothing ->
             assertFailure ("cellAt returned Nothing for in-bounds coord " ++ show coord)
           Just (Empty c) ->
             assertEqual "candidates" expected c
           Just (Fixed _) ->
             assertFailure "expected Empty cell"
      )
      (allCoordinates actual)

testEmptyGridBounds16x16 :: TestTree
testEmptyGridBounds16x16 =
  testCase "emptyGrid 16x16 has bounds ((0,0),(15,15))" $ do
    -- given
    let allowedChars   = ['1'..'9'] ++ ['A'..'G']
        expectedBounds = ((0,0),(15,15))

    allowed <- requireSymbols "mkSymbols failed for 16x16 alphabet" allowedChars

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 16x16 symbols" allowed

    -- then
    assertEqual "bounds" expectedBounds (boundsOf actual)

testEmptyGridCandidates16x16 :: TestTree
testEmptyGridCandidates16x16 =
  testCase "emptyGrid 16x16 has full candidate set in every cell" $ do
    -- given
    let allowedChars = ['1'..'9'] ++ ['A'..'G']

    allowed <- requireSymbols "mkSymbols failed for 16x16 alphabet" allowedChars

    let expected = S.fromList (mapMaybe (mkSymbol allowed) allowedChars)

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 16x16 symbols" allowed

    -- then
    mapM_
      (\coord ->
         case cellAt actual coord of
           Nothing ->
             assertFailure ("cellAt returned Nothing for in-bounds coord " ++ show coord)
           Just (Empty c) ->
             assertEqual "candidates" expected c
           Just (Fixed _) ->
             assertFailure "expected Empty cell"
      )
      (allCoordinates actual)

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
-- sideLength
----------------------------------------------------------------------

testSideLength4x4 :: TestTree
testSideLength4x4 =
  testCase "sideLength is 4 for ['1'..'4']" $ do
    -- given
    let allowedChars = ['1'..'4']
        expected     = 4

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    -- when
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
    let actual = sideLength grid

    -- then
    assertEqual "sideLength" expected actual

testSideLength9x9 :: TestTree
testSideLength9x9 =
  testCase "sideLength is 9 for ['1'..'9']" $ do
    -- given
    let allowedChars = ['1'..'9']
        expected     = 9

    allowed <- requireSymbols "mkSymbols failed for ['1'..'9']" allowedChars

    -- when
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 9x9 symbols" allowed
    let actual = sideLength grid

    -- then
    assertEqual "sideLength" expected actual

testSideLength16x16 :: TestTree
testSideLength16x16 =
  testCase "sideLength is 16 for 16x16 alphabet" $ do
    -- given
    let allowedChars = ['1'..'9'] ++ ['A'..'G']
        expected     = 16

    allowed <- requireSymbols "mkSymbols failed for 16x16 alphabet" allowedChars

    -- when
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 16x16 symbols" allowed
    let actual = sideLength grid

    -- then
    assertEqual "sideLength" expected actual

----------------------------------------------------------------------
-- setCell
----------------------------------------------------------------------

testSetCellUpdatesOnlyTarget :: TestTree
testSetCellUpdatesOnlyTarget =
  testCase "setCell updates only the target coordinate" $ do
    -- given
    let allowedChars = ['1'..'4']
        coordinate  = (1,2)

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid    <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
    symbol  <- requireSymbol "mkSymbol failed for '2'" allowed '2'

    let cellToSet               = Fixed symbol
        expectedUnchangedCoords = filter (/= coordinate) (allCoordinates grid)

    -- when
    let actual = setCell grid coordinate cellToSet

    -- then
    assertEqual "cell updated" (Just cellToSet) (cellAt actual coordinate)

    mapM_
      (\coord ->
         assertEqual
           ("unchanged at " ++ show coord)
           (cellAt grid coord)
           (cellAt actual coord)
      )
      expectedUnchangedCoords
