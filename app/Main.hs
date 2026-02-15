module Main (main) where

import HelloWorld (alwaysReturnTrue)

main :: IO ()
main = do
  putStrLn "sudoku"
  putStrLn ("alwaysReturnTrue = " ++ show alwaysReturnTrue)