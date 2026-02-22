module Main (main) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Exception (IOException)
import Sudoku.Grid
  ( Cell (..),
    Grid,
    allCoordinates,
    cellAt,
  )
import Sudoku.IO.File (readFileSafe)
import Sudoku.Placements (PlacementError)
import Sudoku.PuzzleBuilder (buildPuzzle)
import Sudoku.PuzzleParser (ParseError, parsePuzzle)
import Sudoku.PuzzleSolver
  ( PuzzleError,
    SolveResult (..),
    solve,
  )

----------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------

examplePuzzlePath :: FilePath
examplePuzzlePath =
  "puzzles/valid/25x25/classic-25x25-easy-486b1792a9b027e05f2c539290e2548c.sdk"

data AppError
  = FileError IOException
  | ParseError ParseError
  | BuildError PlacementError
  | SolveError PuzzleError
  deriving (Show)

----------------------------------------------------------------------
-- Main
----------------------------------------------------------------------

main :: IO ()
main = do
  result <- run examplePuzzlePath
  case result of
    Left err -> putStrLn ("Error: " ++ show err)
    Right (grid, progressPct) -> do
      putStrLn ("Solving progress: " ++ show progressPct ++ "%")
      print grid

run :: FilePath -> IO (Either AppError (Grid, Double))
run path = do
  fileResult <- readFileSafe path
  pure $ do
    content <- first FileError fileResult
    (symbols, placements) <- first ParseError (parsePuzzle Nothing content)
    built <- first BuildError (buildPuzzle symbols placements)

    let emptiesBefore = countEmptyCells built

    solveResult <- first SolveError (solve built)
    let solvedGrid =
          case solveResult of
            Solved g -> g
            Unsolvable g -> g

    let emptiesAfter = countEmptyCells solvedGrid
        progressPct = progressPercent emptiesBefore emptiesAfter

    Right (solvedGrid, progressPct)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

first :: (e -> e') -> Either e a -> Either e' a
first f = either (Left . f) Right

countEmptyCells :: Grid -> Int
countEmptyCells grid =
  length
    [ ()
    | coord <- allCoordinates grid,
      isEmpty (cellAt grid coord)
    ]
  where
    isEmpty = \case
      Just (Empty _) -> True
      _ -> False

progressPercent :: Int -> Int -> Double
progressPercent emptiesBefore emptiesAfter
  | emptiesBefore <= 0 = 100.0
  | otherwise =
      let filled = emptiesBefore - emptiesAfter
       in (fromIntegral filled / fromIntegral emptiesBefore) * 100.0
