module Main (main) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Sudoku.App.Options (parseArgs, usage)
import Sudoku.App.Runner (runWithOptions)
import System.Environment (getArgs)
import System.Exit (exitFailure)

----------------------------------------------------------------------
-- Main
----------------------------------------------------------------------

main :: IO ()
main = do
  argv <- getArgs
  case parseArgs argv of
    Left msg -> do
      putStrLn msg
      if msg == usage
        then pure ()
        else putStrLn "" >> putStrLn usage >> exitFailure
    Right opts -> do
      result <- runWithOptions opts
      case result of
        Left err -> do
          putStrLn ("Error: " <> show err)
          exitFailure
        Right () -> pure ()
