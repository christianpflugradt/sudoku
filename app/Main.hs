module Main (main) where

import Control.Exception (IOException)

import Sudoku.Grid (Grid, PlacementError)
import Sudoku.IO.File (readFileSafe)
import Sudoku.PuzzleParser (ParseError, parsePuzzle)
import Sudoku.PuzzleBuilder (buildPuzzle)

----------------------------------------------------------------------
-- * Configuration
----------------------------------------------------------------------

examplePuzzlePath :: FilePath
examplePuzzlePath = "puzzles/valid/25x25/classic-25x25-easy-486b1792a9b027e05f2c539290e2548c.sdk"

data AppError
  = FileError IOException
  | ParseError ParseError
  | BuildError PlacementError
  deriving (Show)

----------------------------------------------------------------------
-- * Main
----------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "sudoku"

  result <- run examplePuzzlePath
  case result of
    Left err  -> putStrLn ("Error: " ++ show err)
    Right grid -> print grid

run :: FilePath -> IO (Either AppError Grid)
run path = do
  fileResult <- readFileSafe path
  pure $ do
    content <- first FileError fileResult
    (symbols, placements) <- first ParseError (parsePuzzle Nothing content)
    first BuildError (buildPuzzle symbols placements)

first :: (e -> e') -> Either e a -> Either e' a
first f = either (Left . f) Right
