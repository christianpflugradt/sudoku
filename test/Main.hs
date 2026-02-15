module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import HelloWorld (alwaysReturnTrue)

main :: IO ()
main =
  defaultMain $
    testGroup "sudoku"
      [ testCase "alwaysReturnTrue returns True" $
          assertBool "expected True" alwaysReturnTrue
      ]
