module Sudoku.GridSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure)

import qualified Data.Set as S
import Data.Maybe (mapMaybe)

import Sudoku.Grid
  ( emptyGrid
  , Cell(..)
  , sideLength
  , boundsOf
  , cellAt
  , allCoordinates
  , rowOf
  , colOf
  , boxOf
  )

import Sudoku.Symbols
  ( mkSymbol
  )

import Sudoku.TestHelpers
  ( requireSymbols
  , requireEmptyGrid
  )

----------------------------------------------------------------------
-- Test suite
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Grid"
    [ testGroup "emptyGrid"
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
    , testGroup "rowOf"
        [ testRowOf4x4Middle
        , testRowOf9x9Boundary
        ]
    , testGroup "colOf"
        [ testColOf4x4Middle
        , testColOf16x16Boundary
        ]
    , testGroup "boxOf"
        [ testBoxOf4x4TopLeft
        , testBoxOf4x4BottomRight
        , testBoxOf9x9Center
        , testBoxOf16x16NonTrivial
        ]
    ]

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
-- rowOf
----------------------------------------------------------------------

testRowOf4x4Middle :: TestTree
testRowOf4x4Middle =
  testCase "rowOf 4x4 returns the correct row for (1,2)" $ do
    -- given
    let allowedChars = ['1'..'4']
        coord = (1,2)
        expected =
          [ (0,2)
          , (1,2)
          , (2,2)
          , (3,2)
          ]

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid    <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when
    let actual = rowOf grid coord

    -- then
    assertEqual "rowOf" expected actual

testRowOf9x9Boundary :: TestTree
testRowOf9x9Boundary =
  testCase "rowOf 9x9 returns the correct row for (8,0)" $ do
    -- given
    let allowedChars = ['1'..'9']
        coord = (8,0)
        expected =
          [ (0,0)
          , (1,0)
          , (2,0)
          , (3,0)
          , (4,0)
          , (5,0)
          , (6,0)
          , (7,0)
          , (8,0)
          ]

    allowed <- requireSymbols "mkSymbols failed for ['1'..'9']" allowedChars
    grid    <- requireEmptyGrid "emptyGrid returned Nothing for 9x9 symbols" allowed

    -- when
    let actual = rowOf grid coord

    -- then
    assertEqual "rowOf" expected actual

----------------------------------------------------------------------
-- colOf
----------------------------------------------------------------------

testColOf4x4Middle :: TestTree
testColOf4x4Middle =
  testCase "colOf 4x4 returns the correct column for (1,2)" $ do
    -- given
    let allowedChars = ['1'..'4']
        coord = (1,2)
        expected =
          [ (1,0)
          , (1,1)
          , (1,2)
          , (1,3)
          ]

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid    <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when
    let actual = colOf grid coord

    -- then
    assertEqual "colOf" expected actual

testColOf16x16Boundary :: TestTree
testColOf16x16Boundary =
  testCase "colOf 16x16 returns the correct column for (0,15)" $ do
    -- given
    let allowedChars = ['1'..'9'] ++ ['A'..'G']
        coord = (0,15)
        expected =
          [ (0,0)
          , (0,1)
          , (0,2)
          , (0,3)
          , (0,4)
          , (0,5)
          , (0,6)
          , (0,7)
          , (0,8)
          , (0,9)
          , (0,10)
          , (0,11)
          , (0,12)
          , (0,13)
          , (0,14)
          , (0,15)
          ]

    allowed <- requireSymbols "mkSymbols failed for 16x16 alphabet" allowedChars
    grid    <- requireEmptyGrid "emptyGrid returned Nothing for 16x16 symbols" allowed

    -- when
    let actual = colOf grid coord

    -- then
    assertEqual "colOf" expected actual

----------------------------------------------------------------------
-- boxOf
----------------------------------------------------------------------

testBoxOf4x4TopLeft :: TestTree
testBoxOf4x4TopLeft =
  testCase "boxOf 4x4 returns the top-left 2x2 box for (1,0)" $ do
    -- given
    let allowedChars = ['1'..'4']
        coord = (1,0)
        expected =
          [ (0,0)
          , (0,1)
          , (1,0)
          , (1,1)
          ]

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid    <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when
    let actual = boxOf grid coord

    -- then
    assertEqual "boxOf" expected actual

testBoxOf4x4BottomRight :: TestTree
testBoxOf4x4BottomRight =
  testCase "boxOf 4x4 returns the bottom-right 2x2 box for (3,3)" $ do
    -- given
    let allowedChars = ['1'..'4']
        coord = (3,3)
        expected =
          [ (2,2)
          , (2,3)
          , (3,2)
          , (3,3)
          ]

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid    <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when
    let actual = boxOf grid coord

    -- then
    assertEqual "boxOf" expected actual

testBoxOf9x9Center :: TestTree
testBoxOf9x9Center =
  testCase "boxOf 9x9 returns the center 3x3 box for (4,4)" $ do
    -- given
    let allowedChars = ['1'..'9']
        coord = (4,4)
        expected =
          [ (3,3)
          , (3,4)
          , (3,5)
          , (4,3)
          , (4,4)
          , (4,5)
          , (5,3)
          , (5,4)
          , (5,5)
          ]

    allowed <- requireSymbols "mkSymbols failed for ['1'..'9']" allowedChars
    grid    <- requireEmptyGrid "emptyGrid returned Nothing for 9x9 symbols" allowed

    -- when
    let actual = boxOf grid coord

    -- then
    assertEqual "boxOf" expected actual

testBoxOf16x16NonTrivial :: TestTree
testBoxOf16x16NonTrivial =
  testCase "boxOf 16x16 returns the correct 4x4 box for (6,10)" $ do
    -- given
    let allowedChars = ['1'..'9'] ++ ['A'..'G']
        coord = (6,10)
        expected =
          [ (4,8)
          , (4,9)
          , (4,10)
          , (4,11)
          , (5,8)
          , (5,9)
          , (5,10)
          , (5,11)
          , (6,8)
          , (6,9)
          , (6,10)
          , (6,11)
          , (7,8)
          , (7,9)
          , (7,10)
          , (7,11)
          ]

    allowed <- requireSymbols "mkSymbols failed for 16x16 alphabet" allowedChars
    grid    <- requireEmptyGrid "emptyGrid returned Nothing for 16x16 symbols" allowed

    -- when
    let actual = boxOf grid coord

    -- then
    assertEqual "boxOf" expected actual
