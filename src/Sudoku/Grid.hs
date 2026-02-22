module Sudoku.Grid
  ( Candidates,
    Cell (..),
    Coordinate,
    Grid,
    PlacementError,
    Placements,
    Unit,
    allCoordinates,
    boundsOf,
    cellAt,
    emptyGrid,
    setCell,
    sideLength,
  )
where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Monad (foldM, when)
import Data.Array (Array, (!), (//))
import qualified Data.Array as A
import qualified Data.Set as S
import Sudoku.Geometry
  ( Coordinate,
    SideLength (..),
    Unit,
  )
import qualified Sudoku.Geometry as G
import Sudoku.Placements (PlacementError (..), Placements)
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
    cells :: Array Coordinate Cell
  }
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

emptyGrid :: Symbols -> Maybe Grid
emptyGrid symbols
  | perfectSquare =
      Just
        Grid
          { allowed = symbols,
            cells = A.array ((0, 0), (upper, upper)) [(k, value) | k <- keys]
          }
  | otherwise = Nothing
  where
    n = length (symbolsList symbols)
    b = floor (sqrt (fromIntegral n :: Double))
    perfectSquare = b * b == n
    upper = n - 1
    value = Empty (allCandidates symbols)
    keys = [(x, y) | x <- [0 .. upper], y <- [0 .. upper]]

sideLength :: Grid -> Int
sideLength grid = length (symbolsList (allowed grid))

boundsOf :: Grid -> (Coordinate, Coordinate)
boundsOf grid = ((0, 0), (upper, upper))
  where
    upper = sideLength grid - 1

allCoordinates :: Grid -> [Coordinate]
allCoordinates grid = [(x, y) | x <- [0 .. upper], y <- [0 .. upper]]
  where
    upper = sideLength grid - 1

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
  let peers = G.peersOf (SideLength (sideLength grid)) coord
  when (any (\c -> cellAt grid c == Just (Fixed symbol)) peers) $
    Left DuplicateInUnit
  let updatedGrid = setCellValue grid coord (Fixed symbol)
  foldM (removeCandidateFromPeer symbol) updatedGrid peers

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
