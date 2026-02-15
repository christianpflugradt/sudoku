module Main (main) where

import Sudoku.Grid
  ( mkSymbols
  , emptyGrid
  )

main :: IO ()
main = do
  putStrLn "sudoku"

  case mkSymbols ['1'..'4'] of
    Nothing ->
      putStrLn "Failed to create symbols."

    Just symbols ->
      case emptyGrid symbols of
        Nothing ->
          putStrLn "Failed to create grid."

        Just grid ->
          print grid
