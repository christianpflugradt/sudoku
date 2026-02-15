module Sudoku.UnitsSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)

import Sudoku.Grid
  ( Cell(Fixed)
  , setCell
  )

import Sudoku.Units
  ( rowOf
  , colOf
  , boxOf
  , inUnitBy
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
  testGroup "Units"
    [ testGroup "rowOf"
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
    , testGroup "inUnitBy"
        [ testInUnitByRowTrueAndFalse
        , testInUnitByColTrueAndFalse
        , testInUnitByBoxTrueAndFalse
        ]

    ]

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

----------------------------------------------------------------------
-- inUnitBy
----------------------------------------------------------------------

testInUnitByRowTrueAndFalse :: TestTree
testInUnitByRowTrueAndFalse =
  testCase "inUnitBy rowOf: True for correct row, False for other row" $ do
    -- given
    let allowedChars = ['1'..'4']
        placedAt     = (3,1)
        queryInRow   = (0,1)  -- same row y=1
        queryOther   = (0,0)  -- different row y=0

    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    baseGrid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" symbols
    symbol <- requireSymbol "mkSymbol failed for '2'" symbols '2'

    let grid = setCell baseGrid placedAt (Fixed symbol)

    -- when
    let actualTrue  = inUnitBy rowOf grid queryInRow symbol
        actualFalse = inUnitBy rowOf grid queryOther symbol

    -- then
    assertEqual "in correct row" True actualTrue
    assertEqual "in other row"   False actualFalse


testInUnitByColTrueAndFalse :: TestTree
testInUnitByColTrueAndFalse =
  testCase "inUnitBy colOf: True for correct col, False for other col" $ do
    -- given
    let allowedChars = ['1'..'4']
        placedAt     = (2,0)
        queryInCol   = (2,3)  -- same col x=2
        queryOther   = (1,3)  -- different col x=1

    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    baseGrid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" symbols
    symbol <- requireSymbol "mkSymbol failed for '3'" symbols '3'

    let grid = setCell baseGrid placedAt (Fixed symbol)

    -- when
    let actualTrue  = inUnitBy colOf grid queryInCol symbol
        actualFalse = inUnitBy colOf grid queryOther symbol

    -- then
    assertEqual "in correct col" True actualTrue
    assertEqual "in other col"   False actualFalse


testInUnitByBoxTrueAndFalse :: TestTree
testInUnitByBoxTrueAndFalse =
  testCase "inUnitBy boxOf: True for correct box, False for other box" $ do
    -- given
    let allowedChars = ['1'..'4']
        placedAt     = (1,0)  -- top-left 2x2 box
        queryInBox   = (0,1)  -- same box
        queryOther   = (3,3)  -- bottom-right box

    symbols <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    baseGrid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" symbols
    symbol <- requireSymbol "mkSymbol failed for '4'" symbols '4'

    let grid = setCell baseGrid placedAt (Fixed symbol)

    -- when
    let actualTrue  = inUnitBy boxOf grid queryInBox symbol
        actualFalse = inUnitBy boxOf grid queryOther symbol

    -- then
    assertEqual "in correct box" True actualTrue
    assertEqual "in other box"   False actualFalse
