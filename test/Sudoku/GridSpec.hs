module Sudoku.GridSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import qualified Data.Set as S
import Sudoku.Geometry (SideLength (..))
import qualified Sudoku.Geometry as G
import Sudoku.Grid
  ( Cell (..),
    allCoordinates,
    boundsOf,
    cellAt,
    emptyGrid,
    setCell,
    sideLength,
  )
import Sudoku.Placements (PlacementError (..))
import Sudoku.Symbols (symbolsList)
import Sudoku.TestHelpers
  ( requireEmptyGrid,
    requireSymbol,
    requireSymbols,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

----------------------------------------------------------------------
-- Test suite
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Sudoku.Grid"
    [ testEmptyGrid,
      testSideLength,
      testCellAt,
      testAllCoordinates,
      testSetCell
    ]

----------------------------------------------------------------------
-- emptyGrid
----------------------------------------------------------------------

testEmptyGrid :: TestTree
testEmptyGrid =
  testGroup
    "emptyGrid"
    [ testEmptyGridBounds4x4,
      testEmptyGridCandidates4x4,
      testEmptyGridBounds9x9,
      testEmptyGridCandidates9x9,
      testEmptyGridBounds16x16,
      testEmptyGridCandidates16x16,
      testEmptyGrid1x1Just,
      testEmptyGrid2SymbolsNothing,
      testEmptyGrid3SymbolsNothing,
      testEmptyGrid6SymbolsNothing
    ]

testEmptyGridBounds4x4 :: TestTree
testEmptyGridBounds4x4 =
  testCase "emptyGrid ['1'..'4'] has 4x4 bounds" $ do
    -- given
    let allowedChars = ['1' .. '4']
        expectedBounds = ((0, 0), (3, 3))

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- then
    assertEqual "bounds" expectedBounds (boundsOf actual)

testEmptyGridCandidates4x4 :: TestTree
testEmptyGridCandidates4x4 =
  testCase "emptyGrid ['1'..'4'] has full candidate set in every cell" $ do
    -- given
    let allowedChars = ['1' .. '4']

    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    let expected = S.fromList (symbolsList allowed)

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- then
    mapM_
      ( \coord ->
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
    let allowedChars = ['1' .. '9']
        expectedBounds = ((0, 0), (8, 8))

    allowed <- requireSymbols "mkSymbols failed for ['1'..'9']" allowedChars

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 9x9 symbols" allowed

    -- then
    assertEqual "bounds" expectedBounds (boundsOf actual)

testEmptyGridCandidates9x9 :: TestTree
testEmptyGridCandidates9x9 =
  testCase "emptyGrid ['1'..'9'] has full candidate set in every cell" $ do
    -- given
    let allowedChars = ['1' .. '9']

    allowed <- requireSymbols "mkSymbols failed for ['1'..'9']" allowedChars
    let expected = S.fromList (symbolsList allowed)

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 9x9 symbols" allowed

    -- then
    mapM_
      ( \coord ->
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
    let allowedChars = ['1' .. '9'] ++ ['A' .. 'G']
        expectedBounds = ((0, 0), (15, 15))

    allowed <- requireSymbols "mkSymbols failed for 16x16 alphabet" allowedChars

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 16x16 symbols" allowed

    -- then
    assertEqual "bounds" expectedBounds (boundsOf actual)

testEmptyGridCandidates16x16 :: TestTree
testEmptyGridCandidates16x16 =
  testCase "emptyGrid 16x16 has full candidate set in every cell" $ do
    -- given
    let allowedChars = ['1' .. '9'] ++ ['A' .. 'G']

    allowed <- requireSymbols "mkSymbols failed for 16x16 alphabet" allowedChars
    let expected = S.fromList (symbolsList allowed)

    -- when
    actual <- requireEmptyGrid "emptyGrid returned Nothing for 16x16 symbols" allowed

    -- then
    mapM_
      ( \coord ->
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
      Just _ -> pure ()
      Nothing -> assertFailure "expected Just grid"

testEmptyGrid2SymbolsNothing :: TestTree
testEmptyGrid2SymbolsNothing =
  testCase "emptyGrid with 2 symbols returns Nothing" $ do
    -- given
    let allowedChars = ['1', '2']
    allowed <- requireSymbols "mkSymbols failed for ['1','2']" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    assertEqual "emptyGrid" Nothing actual

testEmptyGrid3SymbolsNothing :: TestTree
testEmptyGrid3SymbolsNothing =
  testCase "emptyGrid with 3 symbols returns Nothing" $ do
    -- given
    let allowedChars = ['1', '2', '3']
    allowed <- requireSymbols "mkSymbols failed for ['1','2','3']" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    assertEqual "emptyGrid" Nothing actual

testEmptyGrid6SymbolsNothing :: TestTree
testEmptyGrid6SymbolsNothing =
  testCase "emptyGrid with 6 symbols returns Nothing" $ do
    -- given
    let allowedChars = ['1', '2', '3', '4', '5', '6']
    allowed <- requireSymbols "mkSymbols failed for 6 symbols" allowedChars

    -- when
    let actual = emptyGrid allowed

    -- then
    assertEqual "emptyGrid" Nothing actual

----------------------------------------------------------------------
-- sideLength
----------------------------------------------------------------------

testSideLength :: TestTree
testSideLength =
  testGroup
    "sideLength"
    [ testSideLength4x4,
      testSideLength9x9,
      testSideLength16x16
    ]

testSideLength4x4 :: TestTree
testSideLength4x4 =
  testCase "sideLength is 4 for ['1'..'4']" $ do
    -- given
    let allowedChars = ['1' .. '4']
        expected = 4
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
    let allowedChars = ['1' .. '9']
        expected = 9
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
    let allowedChars = ['1' .. '9'] ++ ['A' .. 'G']
        expected = 16
    allowed <- requireSymbols "mkSymbols failed for 16x16 alphabet" allowedChars

    -- when
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 16x16 symbols" allowed
    let actual = sideLength grid

    -- then
    assertEqual "sideLength" expected actual

----------------------------------------------------------------------
-- cellAt
----------------------------------------------------------------------

testCellAt :: TestTree
testCellAt =
  testGroup
    "cellAt"
    [ testCellAtInBoundsJust,
      testCellAtOutOfBoundsNothing
    ]

testCellAtInBoundsJust :: TestTree
testCellAtInBoundsJust =
  testCase "cellAt returns Just for in-bounds coordinate" $ do
    -- given
    let allowedChars = ['1' .. '4']
        coord = (0, 0)
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when
    let actual = cellAt grid coord

    -- then
    case actual of
      Just (Empty _) -> pure ()
      Just (Fixed _) -> assertFailure "expected Empty cell"
      Nothing -> assertFailure "expected Just cell"

testCellAtOutOfBoundsNothing :: TestTree
testCellAtOutOfBoundsNothing =
  testCase "cellAt returns Nothing for out-of-bounds coordinate" $ do
    -- given
    let allowedChars = ['1' .. '4']
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when/then
    assertEqual "(-1,0)" Nothing (cellAt grid (-1, 0))
    assertEqual "(0,-1)" Nothing (cellAt grid (0, -1))
    assertEqual "(4,0)" Nothing (cellAt grid (4, 0))
    assertEqual "(0,4)" Nothing (cellAt grid (0, 4))

----------------------------------------------------------------------
-- allCoordinates
----------------------------------------------------------------------

testAllCoordinates :: TestTree
testAllCoordinates =
  testGroup
    "allCoordinates"
    [ testAllCoordinatesSize4x4,
      testAllCoordinatesCorners4x4,
      testAllCoordinatesAllInBounds4x4,
      testAllCoordinatesNoDuplicates4x4
    ]

testAllCoordinatesSize4x4 :: TestTree
testAllCoordinatesSize4x4 =
  testCase "allCoordinates has length n*n for 4x4" $ do
    -- given
    let allowedChars = ['1' .. '4']
        expected = 16
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when
    let actual = length (allCoordinates grid)

    -- then
    assertEqual "count" expected actual

testAllCoordinatesCorners4x4 :: TestTree
testAllCoordinatesCorners4x4 =
  testCase "allCoordinates includes (0,0) and (3,3) for 4x4" $ do
    -- given
    let allowedChars = ['1' .. '4']
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when
    let coords = allCoordinates grid

    -- then
    assertBool "(0,0) present" ((0, 0) `elem` coords)
    assertBool "(3,3) present" ((3, 3) `elem` coords)

testAllCoordinatesAllInBounds4x4 :: TestTree
testAllCoordinatesAllInBounds4x4 =
  testCase "allCoordinates only contains in-bounds coordinates" $ do
    -- given
    let allowedChars = ['1' .. '4']
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when/then
    mapM_
      ( \coord ->
          case cellAt grid coord of
            Nothing -> assertFailure ("Expected in-bounds coordinate, got " ++ show coord)
            Just _ -> pure ()
      )
      (allCoordinates grid)

testAllCoordinatesNoDuplicates4x4 :: TestTree
testAllCoordinatesNoDuplicates4x4 =
  testCase "allCoordinates contains no duplicates (4x4)" $ do
    -- given
    let allowedChars = ['1' .. '4']
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    -- when
    let coords = allCoordinates grid
        unique = S.fromList coords

    -- then
    assertEqual "unique size" (length coords) (S.size unique)

----------------------------------------------------------------------
-- setCell
----------------------------------------------------------------------

testSetCell :: TestTree
testSetCell =
  testGroup
    "setCell"
    [ testSetCellOutOfBounds,
      testSetCellAlreadySet,
      testSetCellDuplicateInUnitRow,
      testSetCellSetsFixed,
      testSetCellRemovesCandidateFromPeers,
      testSetCellDoesNotTouchNonPeerCandidates,
      testSetCellNoCandidates
    ]

testSetCellOutOfBounds :: TestTree
testSetCellOutOfBounds =
  testCase "setCell returns Left OutOfBounds for out-of-bounds coordinate" $ do
    -- given
    let allowedChars = ['1' .. '4']
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
    sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'

    -- when
    let actual = setCell grid (4, 0) sym1

    -- then
    assertEqual "setCell" (Left OutOfBounds) actual

testSetCellAlreadySet :: TestTree
testSetCellAlreadySet =
  testCase "setCell returns Left AlreadySet when setting a fixed cell again" $ do
    -- given
    let allowedChars = ['1' .. '4']
        coord = (0, 0)
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
    sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'
    sym2 <- requireSymbol "mkSymbol failed for '2'" allowed '2'

    -- when
    grid' <- case setCell grid coord sym1 of
      Left e -> assertFailure ("expected Right, got " ++ show e) >> error "unreachable"
      Right g -> pure g
    let actual = setCell grid' coord sym2

    -- then
    assertEqual "setCell" (Left AlreadySet) actual

testSetCellDuplicateInUnitRow :: TestTree
testSetCellDuplicateInUnitRow =
  testCase "setCell returns Left DuplicateInUnit for duplicate symbol in a row" $ do
    -- given
    let allowedChars = ['1' .. '4']
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
    sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'

    -- when
    grid' <- case setCell grid (0, 0) sym1 of
      Left e -> assertFailure ("expected Right, got " ++ show e) >> error "unreachable"
      Right g -> pure g
    let actual = setCell grid' (1, 0) sym1

    -- then
    assertEqual "setCell" (Left DuplicateInUnit) actual

testSetCellSetsFixed :: TestTree
testSetCellSetsFixed =
  testCase "setCell sets the target cell to Fixed symbol" $ do
    -- given
    let allowedChars = ['1' .. '4']
        coord = (0, 0)
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
    sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'

    -- when
    grid' <- case setCell grid coord sym1 of
      Left e -> assertFailure ("expected Right, got " ++ show e) >> error "unreachable"
      Right g -> pure g

    -- then
    assertEqual "cellAt" (Just (Fixed sym1)) (cellAt grid' coord)

testSetCellRemovesCandidateFromPeers :: TestTree
testSetCellRemovesCandidateFromPeers =
  testCase "setCell removes the symbol from candidate sets of all peers" $ do
    -- given
    let allowedChars = ['1' .. '4']
        coord = (0, 0)
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
    sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'

    let peers = G.peersOf (SideLength (sideLength grid)) coord

    -- when
    grid' <- case setCell grid coord sym1 of
      Left e -> assertFailure ("expected Right, got " ++ show e) >> error "unreachable"
      Right g -> pure g

    -- then
    mapM_
      ( \p ->
          case cellAt grid' p of
            Nothing ->
              assertFailure ("peer out of bounds? " ++ show p)
            Just (Fixed _) ->
              pure ()
            Just (Empty cands) ->
              assertBool ("expected candidate removed in " ++ show p) (sym1 `S.notMember` cands)
      )
      peers

testSetCellDoesNotTouchNonPeerCandidates :: TestTree
testSetCellDoesNotTouchNonPeerCandidates =
  testCase "setCell does not change candidate set of non-peers" $ do
    -- given
    let allowedChars = ['1' .. '4']
        coord = (0, 0)
        nonPeer = (2, 3)
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed
    sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'
    let expectedFull = S.fromList (symbolsList allowed)

    -- when
    grid' <- case setCell grid coord sym1 of
      Left e -> assertFailure ("expected Right, got " ++ show e) >> error "unreachable"
      Right g -> pure g

    -- then
    case cellAt grid' nonPeer of
      Nothing -> assertFailure "expected in-bounds non-peer"
      Just (Fixed _) -> assertFailure "expected Empty cell"
      Just (Empty c) -> assertEqual "candidates" expectedFull c

testSetCellNoCandidates :: TestTree
testSetCellNoCandidates =
  testCase "setCell returns Left NoCandidates if a peer would lose its last candidate" $ do
    -- given
    let allowedChars = ['1' .. '4']
    allowed <- requireSymbols "mkSymbols failed for ['1'..'4']" allowedChars
    grid0 <- requireEmptyGrid "emptyGrid returned Nothing for 4x4 symbols" allowed

    sym1 <- requireSymbol "mkSymbol failed for '1'" allowed '1'
    sym2 <- requireSymbol "mkSymbol failed for '2'" allowed '2'
    sym3 <- requireSymbol "mkSymbol failed for '3'" allowed '3'
    sym4 <- requireSymbol "mkSymbol failed for '4'" allowed '4'

    grid1 <- case setCell grid0 (2, 0) sym2 of
      Left e -> assertFailure ("setup failed: " ++ show e) >> error "unreachable"
      Right g -> pure g
    grid2 <- case setCell grid1 (1, 2) sym3 of
      Left e -> assertFailure ("setup failed: " ++ show e) >> error "unreachable"
      Right g -> pure g
    grid3 <- case setCell grid2 (0, 1) sym4 of
      Left e -> assertFailure ("setup failed: " ++ show e) >> error "unreachable"
      Right g -> pure g

    -- when
    let actual = setCell grid3 (0, 0) sym1

    -- then
    assertEqual "setCell" (Left NoCandidates) actual
