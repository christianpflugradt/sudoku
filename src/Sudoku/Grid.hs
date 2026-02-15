module Sudoku.Grid
    ( Symbols
    , mkSymbols
    , Symbol
    , mkSymbol
    , Candidates
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
import Data.Maybe (mapMaybe)

----------------------------------------------------------------------
-- * Public Types
----------------------------------------------------------------------

newtype Symbols = Symbols [Char] deriving (Eq, Ord, Show)

newtype Symbol = Symbol Char
  deriving (Eq, Ord, Show)

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

mkSymbols :: [Char] -> Maybe Symbols
mkSymbols [] = Nothing
mkSymbols xs
  | unique    = Just (Symbols xs)
  | otherwise = Nothing
  where
    unique = length xs == S.size (S.fromList xs)

mkSymbol :: Symbols -> Char -> Maybe Symbol
mkSymbol (Symbols xs) c
    | c `elem` xs = Just (Symbol c)
    | otherwise   = Nothing

emptyGrid :: Symbols -> Maybe Grid
emptyGrid symbols@(Symbols xs)
  | perfectSquare = Just (Grid
      { allowed = symbols
      , cells = A.array ((0, 0), (upper, upper)) (map (, value) keys)
      })
  | otherwise     = Nothing
  where
    n = length xs
    b = floor (sqrt (fromIntegral n :: Double))
    perfectSquare = b * b == n
    upper = n - 1
    value = Empty (allCandidates symbols)
    keys  = [ (x, y) | x <- [0..upper], y <- [0..upper] ]

sideLength :: Grid -> Int
sideLength grid = case allowed grid of Symbols xs -> length xs

boundsOf :: Grid -> (Coordinate, Coordinate)
boundsOf grid = ((0, 0), (upper, upper))
  where
    upper = case allowed grid of Symbols xs -> length xs - 1

cellAt :: Grid -> Coordinate -> Maybe Cell
cellAt grid coord
  | A.inRange(A.bounds (cells grid)) coord = Just ((cells grid) ! coord)
  | otherwise                          = Nothing

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
allCandidates symbols@(Symbols xs) = S.fromList (mapMaybe (mkSymbol symbols) xs)
