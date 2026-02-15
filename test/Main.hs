module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Sudoku.GridSpec as GridSpec
import qualified Sudoku.UnitsSpec as UnitsSpec

main :: IO ()
main =
  defaultMain $
    testGroup "sudoku"
      [ GridSpec.tests
      , UnitsSpec.tests
      ]
