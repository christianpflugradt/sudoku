module Sudoku.GeometrySpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Data.List (sort)
import qualified Data.Set as S
import Sudoku.Geometry
  ( Coordinate,
    SideLength (..),
    Unit,
    allUnits,
    peersOf,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

----------------------------------------------------------------------
-- Test suite
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Sudoku.Geometry"
    [ testAllUnits,
      testPeersOf
    ]

----------------------------------------------------------------------
-- allUnits
----------------------------------------------------------------------

testAllUnits :: TestTree
testAllUnits =
  testGroup
    "allUnits"
    [ testAllUnits4x4Count,
      testAllUnits4x4ContainsRow,
      testAllUnits4x4ContainsCol,
      testAllUnits4x4ContainsBox,
      testAllUnits4x4NoDuplicates,
      testAllUnits9x9Count,
      testAllUnits9x9ContainsCenterBox
    ]

testAllUnits4x4Count :: TestTree
testAllUnits4x4Count =
  testCase "allUnits 4x4 returns 12 units (4 rows + 4 cols + 4 boxes)" $ do
    -- given
    let n = SideLength 4

    -- when
    let units = allUnits n

    -- then
    assertEqual "unit count" 12 (length units)

testAllUnits4x4ContainsRow :: TestTree
testAllUnits4x4ContainsRow =
  testCase "allUnits 4x4 contains the expected row for y=2" $ do
    -- given
    let n = SideLength 4
        expected :: Unit
        expected =
          [ (0, 2),
            (1, 2),
            (2, 2),
            (3, 2)
          ]

    -- when
    let units = allUnits n

    -- then
    assertBool "expected row not found" (unitMember expected units)

testAllUnits4x4ContainsCol :: TestTree
testAllUnits4x4ContainsCol =
  testCase "allUnits 4x4 contains the expected column for x=1" $ do
    -- given
    let n = SideLength 4
        expected :: Unit
        expected =
          [ (1, 0),
            (1, 1),
            (1, 2),
            (1, 3)
          ]

    -- when
    let units = allUnits n

    -- then
    assertBool "expected column not found" (unitMember expected units)

testAllUnits4x4ContainsBox :: TestTree
testAllUnits4x4ContainsBox =
  testCase "allUnits 4x4 contains the top-left 2x2 box" $ do
    -- given
    let n = SideLength 4
        expected :: Unit
        expected =
          [ (0, 0),
            (0, 1),
            (1, 0),
            (1, 1)
          ]

    -- when
    let units = allUnits n

    -- then
    assertBool "expected box not found" (unitMember expected units)

testAllUnits4x4NoDuplicates :: TestTree
testAllUnits4x4NoDuplicates =
  testCase "allUnits 4x4 contains no duplicate units" $ do
    -- given
    let n = SideLength 4

    -- when
    let units = canonicalUnits (allUnits n)

    -- then
    assertEqual "duplicates detected" (length units) (S.size (S.fromList units))

testAllUnits9x9Count :: TestTree
testAllUnits9x9Count =
  testCase "allUnits 9x9 returns 27 units (9 rows + 9 cols + 9 boxes)" $ do
    -- given
    let n = SideLength 9

    -- when
    let units = allUnits n

    -- then
    assertEqual "unit count" 27 (length units)

testAllUnits9x9ContainsCenterBox :: TestTree
testAllUnits9x9ContainsCenterBox =
  testCase "allUnits 9x9 contains the center 3x3 box" $ do
    -- given
    let n = SideLength 9
        expected :: Unit
        expected =
          [ (3, 3),
            (3, 4),
            (3, 5),
            (4, 3),
            (4, 4),
            (4, 5),
            (5, 3),
            (5, 4),
            (5, 5)
          ]

    -- when
    let units = allUnits n

    -- then
    assertBool "expected center box not found" (unitMember expected units)

----------------------------------------------------------------------
-- peersOf
----------------------------------------------------------------------

testPeersOf :: TestTree
testPeersOf =
  testGroup
    "peersOf"
    [ testPeersOf4x4MiddleExact,
      testPeersOf4x4ExcludesSelf,
      testPeersOf4x4NoDuplicates,
      testPeersOf9x9CenterCount,
      testPeersOf9x9CenterContainsExamples
    ]

testPeersOf4x4MiddleExact :: TestTree
testPeersOf4x4MiddleExact =
  testCase "peersOf 4x4 returns the correct peers for (1,2)" $ do
    -- given
    let n = SideLength 4
        coord :: Coordinate
        coord = (1, 2)
        expected =
          S.fromList
            [ (0, 2), -- row
              (2, 2),
              (3, 2),
              (1, 0), -- col
              (1, 1),
              (1, 3),
              (0, 3) -- box-only (same 2x2 box, not in row/col)
            ]

    -- when
    let actual = S.fromList (peersOf n coord)

    -- then
    assertEqual "peer set" expected actual
    assertEqual "peer count" 7 (S.size actual)

testPeersOf4x4ExcludesSelf :: TestTree
testPeersOf4x4ExcludesSelf =
  testCase "peersOf 4x4 does not include the coordinate itself" $ do
    -- given
    let n = SideLength 4
        coord = (0, 0)

    -- when
    let peers = peersOf n coord

    -- then
    assertBool "self unexpectedly included" (coord `notElem` peers)

testPeersOf4x4NoDuplicates :: TestTree
testPeersOf4x4NoDuplicates =
  testCase "peersOf 4x4 contains no duplicates" $ do
    -- given
    let n = SideLength 4
        coord = (3, 3)

    -- when
    let peers = peersOf n coord

    -- then
    assertEqual "duplicates detected" (length peers) (S.size (S.fromList peers))

testPeersOf9x9CenterCount :: TestTree
testPeersOf9x9CenterCount =
  testCase "peersOf 9x9 returns 20 peers for (4,4)" $ do
    -- given
    let n = SideLength 9
        coord = (4, 4)

    -- when
    let peers = S.fromList (peersOf n coord)

    -- then
    assertEqual "peer count" 20 (S.size peers)

testPeersOf9x9CenterContainsExamples :: TestTree
testPeersOf9x9CenterContainsExamples =
  testCase "peersOf 9x9 for (4,4) includes expected row/col/box peers" $ do
    -- given
    let n = SideLength 9
        coord = (4, 4)
        expectedSamples =
          [ (0, 4), -- same row
            (4, 0), -- same col
            (3, 3) -- same box
          ]
    -- when
    let peers = S.fromList (peersOf n coord)

    -- then
    mapM_ (\p -> assertBool ("missing peer: " ++ show p) (p `S.member` peers)) expectedSamples
    assertBool "self unexpectedly included" (coord `S.notMember` peers)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

canonicalUnit :: Unit -> Unit
canonicalUnit = sort

canonicalUnits :: [Unit] -> [Unit]
canonicalUnits = map canonicalUnit

unitMember :: Unit -> [Unit] -> Bool
unitMember u us =
  let target = canonicalUnit u
      pool = canonicalUnits us
   in target `elem` pool
