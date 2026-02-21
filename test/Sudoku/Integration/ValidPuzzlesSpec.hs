module Sudoku.Integration.ValidPuzzlesSpec
  ( testParseAllValidPuzzles
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Monad (forM, forM_, when)
import Data.List (sort)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

import Sudoku.PuzzleParser (parsePuzzle)

----------------------------------------------------------------------
-- Test
----------------------------------------------------------------------

testParseAllValidPuzzles :: TestTree
testParseAllValidPuzzles =
  testGroup "integration"
    [ testCase "parses all puzzles/valid/**/*.sdk successfully" $ do
        files <- sort <$> listSdkFilesRecursive "puzzles/valid"
        when (null files) $ assertFailure "No .sdk files found under puzzles/valid"
        forM_ files $ \fp -> do
          input <- readFile fp
          case parsePuzzle Nothing input of
            Left err ->
              assertFailure $ unlines
                [ "Expected Right, but got Left"
                , "File: " ++ fp
                , "Error: " ++ show err
                ]
            Right _ -> pure ()
    ]

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

listSdkFilesRecursive :: FilePath -> IO [FilePath]
listSdkFilesRecursive root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else go root
  where
    go dir = do
      entries <- listDirectory dir
      paths <- forM entries $ \e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        if isDir
          then go p
          else pure [p | takeExtension p == ".sdk"]
      pure (concat paths)
