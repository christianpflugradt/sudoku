module Sudoku.App.Runner
  ( AppError (..),
    runWithOptions,
  )
where

import Control.Exception (IOException)
import Control.Monad (void, when)
import Data.Either (rights)
import Data.List (sort)
import Data.Time.Clock
  ( NominalDiffTime,
    diffUTCTime,
    getCurrentTime,
  )
import Sudoku.App.Options
  ( Mode (..),
    Options (..),
    PrintTarget (..),
    RenderFormat (..),
  )
import Sudoku.App.Render (renderGrid, renderGridCompact)
import Sudoku.Grid
  ( Cell (..),
    Grid,
    allCoordinates,
    cellAt,
  )
import Sudoku.IO.File (readFileSafe)
import Sudoku.PuzzleBuilder (BuildError, buildPuzzle)
import Sudoku.PuzzleParser (ParseError, parsePuzzle)
import Sudoku.Solver.PuzzleSolver
  ( SolveResult (..),
    solve,
  )
import Sudoku.Solver.Strategy (PuzzleError)
import Sudoku.Symbols (Symbols, symbolChar, symbolsList)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

data AppError
  = FileError IOException
  | ParseError ParseError
  | BuildPuzzleError BuildError
  | SolveError PuzzleError
  deriving (Show)

data OutcomeStatus
  = OutcomeSolved
  | OutcomeUnsolved
  | OutcomeError
  deriving (Eq, Show)

data RunOutcome = RunOutcome
  { outStatus :: OutcomeStatus,
    outElapsed :: NominalDiffTime,
    outFilledBefore :: Int,
    outFilledAfter :: Int,
    outEmptyBefore :: Int,
    outEmptyAfter :: Int
  }
  deriving (Eq, Show)

data RunSummary = RunSummary
  { sumFiles :: Int,
    sumSolved :: Int,
    sumUnsolved :: Int,
    sumErrors :: Int,
    sumTotalElapsed :: NominalDiffTime,
    sumTotalEmptyBefore :: Int,
    sumTotalFilledDelta :: Int
  }
  deriving (Eq, Show)

emptySummary :: RunSummary
emptySummary =
  RunSummary
    { sumFiles = 0,
      sumSolved = 0,
      sumUnsolved = 0,
      sumErrors = 0,
      sumTotalElapsed = 0,
      sumTotalEmptyBefore = 0,
      sumTotalFilledDelta = 0
    }

addOutcome :: RunSummary -> RunOutcome -> RunSummary
addOutcome s o =
  s
    { sumFiles = sumFiles s + 1,
      sumSolved = sumSolved s + if outStatus o == OutcomeSolved then 1 else 0,
      sumUnsolved = sumUnsolved s + if outStatus o == OutcomeUnsolved then 1 else 0,
      sumErrors = sumErrors s + if outStatus o == OutcomeError then 1 else 0,
      sumTotalElapsed = sumTotalElapsed s + outElapsed o,
      sumTotalEmptyBefore = sumTotalEmptyBefore s + outEmptyBefore o,
      sumTotalFilledDelta = sumTotalFilledDelta s + (outFilledAfter o - outFilledBefore o)
    }

runWithOptions :: Options -> IO (Either AppError ())
runWithOptions opts =
  case mode opts of
    SingleFile fp -> do
      r <- runOne opts fp
      pure (void r)
    Directory dir -> do
      t0 <- getCurrentTime
      fps <- sort <$> listSdkFilesRecursive dir
      results <- mapM (runOne opts) fps
      t1 <- getCurrentTime
      let totalWall = diffUTCTime t1 t0

      let outcomes = rights results
      let summary = foldl addOutcome emptySummary outcomes

      putStrLn "\n--- summary ---"
      putStrLn ("root:              " <> dir)
      putStrLn ("files:             " <> show (sumFiles summary))
      putStrLn ("solved:            " <> show (sumSolved summary))
      putStrLn ("unsolved:          " <> show (sumUnsolved summary))
      putStrLn ("errors:            " <> show (sumErrors summary))
      putStrLn ("total time (wall): " <> showDuration totalWall)
      putStrLn ("total time (sum):  " <> showDuration (sumTotalElapsed summary))
      putStrLn ("avg time/file:     " <> showSeconds (avgTime summary))
      putStrLn ("progress overall:  " <> showFF1 (overallProgressPct summary) <> "%")
      putStrLn ("filled delta:      " <> show (sumTotalFilledDelta summary))
      putStrLn ("empty before:      " <> show (sumTotalEmptyBefore summary))

      pure (Right ())

avgTime :: RunSummary -> Double
avgTime s
  | sumFiles s <= 0 = 0
  | otherwise =
      realToFrac (sumTotalElapsed s) / fromIntegral (sumFiles s)

overallProgressPct :: RunSummary -> Double
overallProgressPct s
  | sumTotalEmptyBefore s <= 0 = 0
  | otherwise =
      (fromIntegral (sumTotalFilledDelta s) / fromIntegral (sumTotalEmptyBefore s)) * 100.0

runOne :: Options -> FilePath -> IO (Either AppError RunOutcome)
runOne opts fp = do
  t0 <- getCurrentTime
  fileResult <- readFileSafe fp
  case fileResult of
    Left ioErr -> do
      t1 <- getCurrentTime
      let elapsed = diffUTCTime t1 t0
      putStrLn ("ERROR " <> fp <> ": " <> show ioErr)
      pure $
        Right
          RunOutcome
            { outStatus = OutcomeError,
              outElapsed = elapsed,
              outFilledBefore = 0,
              outFilledAfter = 0,
              outEmptyBefore = 0,
              outEmptyAfter = 0
            }
    Right content -> do
      case parsePuzzle Nothing content of
        Left perr -> do
          t1 <- getCurrentTime
          let elapsed = diffUTCTime t1 t0
          putStrLn ("ERROR " <> fp <> ": " <> show perr)
          pure $
            Right
              RunOutcome
                { outStatus = OutcomeError,
                  outElapsed = elapsed,
                  outFilledBefore = 0,
                  outFilledAfter = 0,
                  outEmptyBefore = 0,
                  outEmptyAfter = 0
                }
        Right (symbols, placements) ->
          case buildPuzzle symbols placements of
            Left berr -> do
              t1 <- getCurrentTime
              let elapsed = diffUTCTime t1 t0
              putStrLn ("ERROR " <> fp <> ": " <> show berr)
              pure $
                Right
                  RunOutcome
                    { outStatus = OutcomeError,
                      outElapsed = elapsed,
                      outFilledBefore = 0,
                      outFilledAfter = 0,
                      outEmptyBefore = 0,
                      outEmptyAfter = 0
                    }
            Right grid0 -> do
              let filledBefore = countFilled grid0
                  emptyBefore = countEmpty grid0
              let solveResult = solve grid0
              t1 <- getCurrentTime
              let elapsed = diffUTCTime t1 t0

              case solveResult of
                Left serr -> do
                  putStrLn ("ERROR " <> fp <> ": " <> show serr)
                  whenStats opts fp filledBefore emptyBefore filledBefore emptyBefore elapsed (Left serr)
                  pure $
                    Right
                      RunOutcome
                        { outStatus = OutcomeError,
                          outElapsed = elapsed,
                          outFilledBefore = filledBefore,
                          outFilledAfter = filledBefore,
                          outEmptyBefore = emptyBefore,
                          outEmptyAfter = emptyBefore
                        }
                Right sr@(Solved grid1) -> do
                  emitOutputs opts fp symbols grid0 sr
                  let filledAfter = countFilled grid1
                      emptyAfter = countEmpty grid1
                  whenStats opts fp filledBefore emptyBefore filledAfter emptyAfter elapsed (Right sr)
                  pure $
                    Right
                      RunOutcome
                        { outStatus = OutcomeSolved,
                          outElapsed = elapsed,
                          outFilledBefore = filledBefore,
                          outFilledAfter = filledAfter,
                          outEmptyBefore = emptyBefore,
                          outEmptyAfter = emptyAfter
                        }
                Right sr@(Unsolvable grid1) -> do
                  emitOutputs opts fp symbols grid0 sr
                  let filledAfter = countFilled grid1
                      emptyAfter = countEmpty grid1
                  whenStats opts fp filledBefore emptyBefore filledAfter emptyAfter elapsed (Right sr)
                  pure $
                    Right
                      RunOutcome
                        { outStatus = OutcomeUnsolved,
                          outElapsed = elapsed,
                          outFilledBefore = filledBefore,
                          outFilledAfter = filledAfter,
                          outEmptyBefore = emptyBefore,
                          outEmptyAfter = emptyAfter
                        }

emitOutputs :: Options -> FilePath -> Symbols -> Grid -> SolveResult -> IO ()
emitOutputs opts fp symbols original solveResult = do
  when (printHeaders opts) $
    do
      putStrLn ("\nfile: " <> fp)
      let n = length (symbolsList symbols)
      putStrLn ("size: " <> show n <> "x" <> show n)
      putStrLn ("symbols: " <> map symbolChar (symbolsList symbols))

  case printTarget opts of
    PrintNone -> pure ()
    PrintOriginal -> printGrid "original" original
    PrintSolution -> printSolutionOnly solveResult
    PrintBoth -> do
      printGrid "original" original
      printSolutionOnly solveResult
  where
    printGrid label grid = do
      putStrLn ("\n--- " <> label <> " ---")
      putStrLn (renderByFormat (renderFormat opts) grid)

    printSolutionOnly sr =
      case sr of
        Solved g -> printGrid "solution" g
        Unsolvable g ->
          when (printPartialSolution opts) $
            printGrid "partial-solution (unsolved)" g

renderByFormat :: RenderFormat -> Grid -> String
renderByFormat fmt =
  case fmt of
    Pretty -> renderGrid
    Compact -> renderGridCompact

whenStats ::
  Options ->
  FilePath ->
  Int ->
  Int ->
  Int ->
  Int ->
  NominalDiffTime ->
  Either PuzzleError SolveResult ->
  IO ()
whenStats opts fp filledBefore emptyBefore filledAfter emptyAfter elapsed result =
  when (printStats opts) $
    do
      let delta = filledAfter - filledBefore
          pct =
            if emptyBefore <= 0
              then
                0.0 :: Double
              else
                (fromIntegral delta / fromIntegral emptyBefore) * 100.0
          status =
            case result of
              Left _ -> "error"
              Right (Solved _) -> "solved"
              Right (Unsolvable _) -> "unsolved"
      putStrLn ("stats: file=" <> fp)
      putStrLn ("  status: " <> status)
      putStrLn ("  filled_before: " <> show filledBefore)
      putStrLn ("  filled_after:  " <> show filledAfter)
      putStrLn ("  filled_delta:  " <> show delta)
      putStrLn ("  empty_before:  " <> show emptyBefore)
      putStrLn ("  empty_after:   " <> show emptyAfter)
      putStrLn ("  progress_pct:  " <> showFF1 pct <> "%")
      putStrLn ("  elapsed:       " <> show elapsed)

showFF1 :: Double -> String
showFF1 x =
  let scaled :: Double
      scaled = fromIntegral (round (x * 10) :: Int) / 10.0
   in show scaled

showFF3 :: Double -> String
showFF3 x =
  let scaled :: Double
      scaled = fromIntegral (round (x * 1000) :: Int) / 1000.0
   in show scaled

showDuration :: NominalDiffTime -> String
showDuration dt = showSeconds (realToFrac dt :: Double)

showSeconds :: Double -> String
showSeconds s
  | s < 1 =
      showFF1 (s * 1000) <> "ms"
  | otherwise =
      showFF3 s <> "s"

countFilled :: Grid -> Int
countFilled grid =
  length
    [ ()
    | c <- allCoordinates grid,
      case cellAt grid c of
        Just (Fixed _) -> True
        _ -> False
    ]

countEmpty :: Grid -> Int
countEmpty grid =
  length
    [ ()
    | c <- allCoordinates grid,
      case cellAt grid c of
        Just (Empty _) -> True
        _ -> False
    ]

listSdkFilesRecursive :: FilePath -> IO [FilePath]
listSdkFilesRecursive root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else go root
  where
    go dir = do
      entries <- listDirectory dir
      paths <-
        mapM
          ( \e -> do
              let p = dir </> e
              isDir <- doesDirectoryExist p
              if isDir
                then go p
                else pure [p | takeExtension p == ".sdk"]
          )
          entries
      pure (concat paths)
