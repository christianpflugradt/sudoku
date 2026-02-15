module Sudoku.Grid
  ( Symbols
  , mkSymbols
  , Symbol
  , Candidates
  , Cell(..)
  , Coordinate
  , Grid
  , emptyGrid
  ) where

import qualified Data.Array as A
import           Data.Array (Array)
import qualified Data.Set as S
import           Data.Maybe (mapMaybe)

----------------------------------------------------------------------
-- * Public Types
----------------------------------------------------------------------

newtype Symbols = Symbols [Char]
  deriving (Eq, Ord, Show)

newtype Symbol = Symbol Char
  deriving (Eq, Ord, Show)

type Candidates = S.Set Symbol

data Cell
  = Fixed Symbol
  | Empty Candidates
  deriving (Eq, Show)

type Coordinate = (Int, Int)

data Grid = Grid
  { allowed :: Symbols
  , cells   :: Array Coordinate Cell
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

emptyGrid :: Symbols -> Maybe Grid
emptyGrid symbols@(Symbols xs)
  | perfectSquare =
      Just
        Grid
          { allowed = symbols
          , cells   = A.array ((0, 0), (upper, upper)) (map (, value) keys)
          }
  | otherwise = Nothing
  where
    n = length xs
    b = floor (sqrt (fromIntegral n :: Double))
    perfectSquare = b * b == n

    upper = n - 1
    value = Empty (allCandidates symbols)
    keys  = [ (x, y) | x <- [0 .. upper], y <- [0 .. upper] ]

----------------------------------------------------------------------
-- * Internal Helpers
----------------------------------------------------------------------

mkSymbol :: Symbols -> Char -> Maybe Symbol
mkSymbol (Symbols allowedChars) c
  | c `elem` allowedChars = Just (Symbol c)
  | otherwise             = Nothing

allCandidates :: Symbols -> Candidates
allCandidates symbols@(Symbols xs) =
  S.fromList (mapMaybe (mkSymbol symbols) xs)
