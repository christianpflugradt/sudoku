module Sudoku.Grid
  ( Candidates,
    Cell (..),
    Coordinate,
    Grid,
    PlacementError,
    Placements,
    SideLength,
    Unit,
    allCoordinates,
    allowedSymbols,
    boundsOf,
    cellAt,
    emptyGrid,
    isComplete,
    setCell,
    sideLength,
    sideLengthInt,
  )
where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Monad (foldM, unless, when)
import Data.Array (Array, (!), (//))
import qualified Data.Array as A
import qualified Data.Set as S
import Sudoku.Geometry
  ( Coordinate,
    SideLength,
    Unit,
    mkSideLength,
    unSideLength,
  )
import qualified Sudoku.Geometry as G
import Sudoku.Placements
  ( PlacementError (..),
    Placements,
  )
import Sudoku.Symbols
  ( Symbol,
    Symbols,
    symbolsList,
  )

----------------------------------------------------------------------
-- Public Types
----------------------------------------------------------------------

type Candidates = S.Set Symbol

data Cell = Fixed Symbol | Empty Candidates
  deriving (Eq, Show)

data Grid = Grid
  { allowed :: Symbols,
    side :: SideLength,
    cells :: Array Coordinate Cell
  }
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

emptyGrid :: Symbols -> Maybe Grid
emptyGrid symbols =
  case mkSideLength n of
    Nothing -> Nothing
    Just side' ->
      Just
        Grid
          { allowed = symbols,
            side = side',
            cells = A.array ((0, 0), (upper, upper)) [(k, value) | k <- keys]
          }
  where
    n = length (symbolsList symbols)
    upper = n - 1
    value = Empty (allCandidates symbols)
    keys = [(x, y) | x <- [0 .. upper], y <- [0 .. upper]]

allowedSymbols :: Grid -> [Symbol]
allowedSymbols = symbolsList . allowed

sideLength :: Grid -> SideLength
sideLength = side

sideLengthInt :: Grid -> Int
sideLengthInt = unSideLength . sideLength

boundsOf :: Grid -> (Coordinate, Coordinate)
boundsOf grid = ((0, 0), (upper, upper))
  where
    upper = sideLengthInt grid - 1

allCoordinates :: Grid -> [Coordinate]
allCoordinates grid = [(x, y) | x <- [0 .. upper], y <- [0 .. upper]]
  where
    upper = sideLengthInt grid - 1

cellAt :: Grid -> Coordinate -> Maybe Cell
cellAt grid coord
  | A.inRange (A.bounds (cells grid)) coord = Just (cells grid ! coord)
  | otherwise = Nothing

setCell :: Grid -> Coordinate -> Symbol -> Either PlacementError Grid
setCell grid coord symbol = do
  case cellAt grid coord of
    Nothing -> Left OutOfBounds
    Just (Fixed _) -> Left AlreadySet
    Just (Empty _) -> Right ()
  unless (symbol `elem` allowedSymbols grid) $
    Left InvalidSymbol
  let peers = G.peersOf (sideLength grid) coord
  when (any (\c -> cellAt grid c == Just (Fixed symbol)) peers) $
    Left DuplicateInUnit
  let updatedGrid = setCellValue grid coord (Fixed symbol)
  foldM (removeCandidateFromPeer symbol) updatedGrid peers

isComplete :: Grid -> Bool
isComplete grid = all isFixed (A.elems (cells grid))
  where
    isFixed (Fixed _) = True
    isFixed (Empty _) = False

----------------------------------------------------------------------
-- Internal Helpers
----------------------------------------------------------------------

allCandidates :: Symbols -> Candidates
allCandidates = S.fromList . symbolsList

removeCandidateFromPeer :: Symbol -> Grid -> Coordinate -> Either PlacementError Grid
removeCandidateFromPeer sym grid coord =
  case cellAt grid coord of
    Nothing -> Left OutOfBounds
    Just (Fixed _) -> Right grid
    Just (Empty candidates) ->
      let candidates' = S.delete sym candidates
       in if S.null candidates'
            then Left NoCandidates
            else Right (setCellValue grid coord (Empty candidates'))

setCellValue :: Grid -> Coordinate -> Cell -> Grid
setCellValue grid coord cell =
  grid {cells = cells grid // [(coord, cell)]}
