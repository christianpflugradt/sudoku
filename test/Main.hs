module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Sudoku.GridSpec as GridSpec

main :: IO ()
main =
  defaultMain $
    testGroup "sudoku"
      [ GridSpec.tests
      ]
