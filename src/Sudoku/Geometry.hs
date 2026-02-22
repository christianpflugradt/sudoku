module Sudoku.Geometry
  ( Coordinate,
    SideLength (..),
    Unit,
    allUnits,
    peersOf,
  )
where

----------------------------------------------------------------------
-- Public Types
----------------------------------------------------------------------

type Coordinate = (Int, Int)

type Unit = [Coordinate]

newtype SideLength = SideLength Int
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

allUnits :: SideLength -> [Unit]
allUnits n@(SideLength side) = rows ++ cols ++ boxes
  where
    rows = [rowOf n (0, y) | y <- [0 .. side - 1]]
    cols = [colOf n (x, 0) | x <- [0 .. side - 1]]
    boxes = [boxOf n (x, y) | x <- [0, boxLength n .. side - 1], y <- [0, boxLength n .. side - 1]]

peersOf :: SideLength -> Coordinate -> [Coordinate]
peersOf n (x, y) = filter (/= (x, y)) $ row ++ col ++ filter (\(bx, by) -> bx /= x && by /= y) box
  where
    row = rowOf n (x, y)
    col = colOf n (x, y)
    box = boxOf n (x, y)

----------------------------------------------------------------------
-- Internal Helpers
----------------------------------------------------------------------

boxLength :: SideLength -> Int
boxLength (SideLength side) = let b = floor (sqrt (fromIntegral side :: Double)) in b

rowOf :: SideLength -> Coordinate -> Unit
rowOf (SideLength side) (_, y) = [(x, y) | x <- [0 .. side - 1]]

colOf :: SideLength -> Coordinate -> Unit
colOf (SideLength side) (x, _) = [(x, y) | y <- [0 .. side - 1]]

boxOf :: SideLength -> Coordinate -> Unit
boxOf n@(SideLength side) (x, y) = [(bx, by) | bx <- [fromX .. toX], by <- [fromY .. toY]]
  where
    b = boxLength n
    fromX = (x `div` b) * b
    fromY = (y `div` b) * b
    toX = min (side - 1) (fromX + b - 1)
    toY = min (side - 1) (fromY + b - 1)
