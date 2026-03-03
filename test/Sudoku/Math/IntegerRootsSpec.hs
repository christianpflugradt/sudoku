module Sudoku.Math.IntegerRootsSpec (tests) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.Math.IntegerRoots (perfectSquareRoot)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

----------------------------------------------------------------------
-- Test suite
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Sudoku.Math.IntegerRoots"
    [ testPerfectSquareRootRejectsNonPositive,
      testPerfectSquareRootAcceptsPerfectSquares,
      testPerfectSquareRootRejectsNonSquares
    ]

testPerfectSquareRootRejectsNonPositive :: TestTree
testPerfectSquareRootRejectsNonPositive =
  testCase "perfectSquareRoot rejects non-positive values" $ do
    assertEqual "0" Nothing (perfectSquareRoot 0)
    assertEqual "-1" Nothing (perfectSquareRoot (-1))
    assertEqual "-16" Nothing (perfectSquareRoot (-16))

testPerfectSquareRootAcceptsPerfectSquares :: TestTree
testPerfectSquareRootAcceptsPerfectSquares =
  testCase "perfectSquareRoot returns the exact root for perfect squares" $ do
    assertEqual "1" (Just 1) (perfectSquareRoot 1)
    assertEqual "4" (Just 2) (perfectSquareRoot 4)
    assertEqual "9" (Just 3) (perfectSquareRoot 9)
    assertEqual "16" (Just 4) (perfectSquareRoot 16)
    assertEqual "2147395600" (Just 46340) (perfectSquareRoot 2147395600)

testPerfectSquareRootRejectsNonSquares :: TestTree
testPerfectSquareRootRejectsNonSquares =
  testCase "perfectSquareRoot rejects non-square values" $ do
    assertEqual "2" Nothing (perfectSquareRoot 2)
    assertEqual "3" Nothing (perfectSquareRoot 3)
    assertEqual "15" Nothing (perfectSquareRoot 15)
    assertEqual "2147395601" Nothing (perfectSquareRoot 2147395601)
