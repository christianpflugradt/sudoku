module Sudoku.IO.File
  ( readFileSafe
  ) where

import Control.Exception (IOException, try)

readFileSafe :: FilePath -> IO (Either IOException String)
readFileSafe path = try (readFile path)
