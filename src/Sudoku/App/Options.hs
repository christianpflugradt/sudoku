module Sudoku.App.Options
  ( Mode (..),
    Options (..),
    PrintTarget (..),
    RenderFormat (..),
    parseArgs,
    usage,
  )
where

import Data.List (isPrefixOf)

data Mode
  = SingleFile FilePath
  | Directory FilePath
  deriving (Eq, Show)

data PrintTarget
  = PrintNone
  | PrintOriginal
  | PrintSolution
  | PrintBoth
  deriving (Eq, Show)

data RenderFormat
  = Pretty
  | Compact
  deriving (Eq, Show)

data Options = Options
  { mode :: Mode,
    printTarget :: PrintTarget,
    renderFormat :: RenderFormat,
    printHeaders :: Bool,
    printStats :: Bool,
    printPartialSolution :: Bool,
    quiet :: Bool
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions =
  Options
    { mode = SingleFile "",
      printTarget = PrintBoth,
      renderFormat = Pretty,
      printHeaders = False,
      printStats = False,
      printPartialSolution = False,
      quiet = False
    }

usage :: String
usage =
  unlines
    [ "Usage:",
      "  sudoku PATH/TO/puzzle.sdk [FLAGS]",
      "  sudoku --dir PATH/TO/PUZZLES [FLAGS]",
      "",
      "Flags:",
      "  --dir PATH                  Solve all .sdk files under PATH (recursive)",
      "  --print=original|solution|both|none",
      "                               What to print (default: both for single-file, none for --dir)",
      "  --format=pretty|compact      Rendering format (default: pretty)",
      "  --headers                    Print basic input info per puzzle",
      "  --stats                      Print statistics per puzzle",
      "  --print-partial-solution     In stuck case, print the partial solution grid",
      "  -q, --quiet                  Suppress grid printing (stats/errors still printed)",
      "  -h, --help                   Show this help"
    ]

parseArgs :: [String] -> Either String Options
parseArgs = go defaultOptions Nothing False
  where
    -- printExplicit = did the user explicitly set --print=...
    go :: Options -> Maybe Mode -> Bool -> [String] -> Either String Options
    go opts minput printExplicit = \case
      [] ->
        case minput of
          Nothing ->
            Left ("Missing puzzle path (or use --dir).\n\n" ++ usage)
          Just m ->
            Right (finalize (opts {mode = m}) m printExplicit)
      ("-h" : _) ->
        Left usage
      ("--help" : _) ->
        Left usage
      ("--dir" : p : xs) ->
        go opts (Just (Directory p)) printExplicit xs
      ("--stats" : xs) ->
        go (opts {printStats = True}) minput printExplicit xs
      ("--headers" : xs) ->
        go (opts {printHeaders = True}) minput printExplicit xs
      ("--print-partial-solution" : xs) ->
        go (opts {printPartialSolution = True}) minput printExplicit xs
      ("-q" : xs) ->
        go (opts {quiet = True}) minput printExplicit xs
      ("--quiet" : xs) ->
        go (opts {quiet = True}) minput printExplicit xs
      ("--format=pretty" : xs) ->
        go (opts {renderFormat = Pretty}) minput printExplicit xs
      ("--format=compact" : xs) ->
        go (opts {renderFormat = Compact}) minput printExplicit xs
      ("--print=original" : xs) ->
        go (opts {printTarget = PrintOriginal}) minput True xs
      ("--print=solution" : xs) ->
        go (opts {printTarget = PrintSolution}) minput True xs
      ("--print=none" : xs) ->
        go (opts {printTarget = PrintNone}) minput True xs
      ("--print=both" : xs) ->
        go (opts {printTarget = PrintBoth}) minput True xs
      (x : xs)
        | "-" `isPrefixOf` x ->
            Left ("Unknown flag: " ++ x ++ "\n\n" ++ usage)
        | otherwise ->
            case minput of
              Nothing -> go opts (Just (SingleFile x)) printExplicit xs
              Just _ ->
                Left ("Unexpected extra argument: " ++ x ++ "\n\n" ++ usage)

    finalize :: Options -> Mode -> Bool -> Options
    finalize o m printExplicit =
      case m of
        SingleFile _ -> o
        Directory _ ->
          if printExplicit
            then o
            else o {printTarget = PrintNone}
