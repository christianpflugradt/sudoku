module Sudoku.Grid
    ( Candidates
    , Cell(..)
    , Coordinate
    , Grid
    , emptyGrid
    , sideLength
    , boundsOf
    , cellAt
    , setCell
    , allCoordinates
    ) where

import qualified Data.Set as S
import Data.Array (Array, (!), (//))
import qualified Data.Array as A

import Sudoku.Symbols
  ( Symbol
  , Symbols
  , symbolsList
  )

----------------------------------------------------------------------
-- * Public Types
----------------------------------------------------------------------

type Candidates = S.Set Symbol

data Cell = Fixed Symbol | Empty Candidates
  deriving (Eq, Show)

type Coordinate = (Int, Int)

data Grid = Grid
  { allowed :: Symbols
  , cells :: Array Coordinate Cell
  } deriving (Eq, Show)

----------------------------------------------------------------------
-- * Public API
----------------------------------------------------------------------

emptyGrid :: Symbols -> Maybe Grid
emptyGrid symbols
  | perfectSquare = Just (Grid
      { allowed = symbols
      , cells = A.array ((0, 0), (upper, upper)) (map (, value) keys)
      })
  | otherwise     = Nothing
  where
    n = length (symbolsList symbols)
    b = floor (sqrt (fromIntegral n :: Double))
    perfectSquare = b * b == n
    upper = n - 1
    value = Empty (allCandidates symbols)
    keys  = [ (x, y) | x <- [0..upper], y <- [0..upper] ]

sideLength :: Grid -> Int
sideLength grid = length (symbolsList (allowed grid))

boundsOf :: Grid -> (Coordinate, Coordinate)
boundsOf grid = ((0, 0), (upper, upper))
  where
    upper = length (symbolsList (allowed grid)) - 1

cellAt :: Grid -> Coordinate -> Maybe Cell
cellAt grid coord
  | A.inRange (A.bounds (cells grid)) coord = Just (cells grid ! coord)
  | otherwise                              = Nothing

setCell :: Grid -> Coordinate -> Cell -> Grid
setCell grid coord cell = grid { cells = cells grid // [(coord, cell)] }

allCoordinates :: Grid -> [Coordinate]
allCoordinates grid = [ (x, y) | x <- [0..upper], y <- [0..upper] ]
  where
    upper = sideLength grid - 1

----------------------------------------------------------------------
-- * Internal Helpers
----------------------------------------------------------------------

allCandidates :: Symbols -> Candidates
allCandidates = S.fromList . symbolsList
