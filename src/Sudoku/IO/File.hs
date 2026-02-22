module Sudoku.IO.File (readFileSafe) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Exception (IOException, try)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

readFileSafe :: FilePath -> IO (Either IOException String)
readFileSafe path = try (readFile path)
