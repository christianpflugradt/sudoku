module Sudoku.Units
    ( Unit
    , rowOf
    , colOf
    , boxOf
    , inUnitBy
    ) where

import Sudoku.Grid
  ( Symbol
  , Cell(Fixed)
  , Coordinate
  , Grid
  , boundsOf
  , sideLength
  , cellAt
  )

----------------------------------------------------------------------
-- * Public Types
----------------------------------------------------------------------

type Unit = [Coordinate]

----------------------------------------------------------------------
-- * Public API
----------------------------------------------------------------------

rowOf :: Grid -> Coordinate -> Unit
rowOf grid (_, coordY) = [(x, coordY) | x <- [0..upperX]]
  where
    ((_, _), (upperX, _)) = boundsOf grid

colOf :: Grid -> Coordinate -> Unit
colOf grid (coordX, _) = [(coordX, y) | y <- [0..upperY]]
  where
    ((_, _), (_, upperY)) = boundsOf grid

boxOf :: Grid -> Coordinate -> Unit
boxOf grid (x, y) = [(boxX, boxY) | boxX <- [fromX..toX], boxY <- [fromY..toY]]
  where
    n = sideLength grid
    b = floor (sqrt (fromIntegral n :: Double))
    fromX = (x `div` b) * b
    fromY = (y `div` b) * b
    toX = fromX + b - 1
    toY = fromY + b - 1

inUnitBy :: (Grid -> Coordinate -> Unit) -> Grid -> Coordinate -> Symbol -> Bool
inUnitBy fn grid coord symbol = any (\c -> cellAt grid c == Just (Fixed symbol)) (fn grid coord)
