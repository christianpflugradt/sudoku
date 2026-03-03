module Sudoku.Geometry
  ( Coordinate,
    SideLength,
    Unit,
    mkSideLength,
    unSideLength,
    allUnits,
    peersOf,
  )
where

----------------------------------------------------------------------
-- Public Types
----------------------------------------------------------------------

import Sudoku.Math.IntegerRoots (perfectSquareRoot)

type Coordinate = (Int, Int)

type Unit = [Coordinate]

newtype SideLength = SideLength Int
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

mkSideLength :: Int -> Maybe SideLength
mkSideLength side
  | side <= 0 = Nothing
  | Just _ <- perfectSquareRoot side = Just (SideLength side)
  | otherwise = Nothing

unSideLength :: SideLength -> Int
unSideLength (SideLength side) = side

allUnits :: SideLength -> [Unit]
allUnits n = rows ++ cols ++ boxes
  where
    side = unSideLength n
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
boxLength n =
  case perfectSquareRoot (unSideLength n) of
    Just b -> b
    Nothing -> error "unreachable: invalid SideLength invariant"

rowOf :: SideLength -> Coordinate -> Unit
rowOf n (_, y) = [(x, y) | x <- [0 .. side - 1]]
  where
    side = unSideLength n

colOf :: SideLength -> Coordinate -> Unit
colOf n (x, _) = [(x, y) | y <- [0 .. side - 1]]
  where
    side = unSideLength n

boxOf :: SideLength -> Coordinate -> Unit
boxOf n (x, y) = [(bx, by) | bx <- [fromX .. toX], by <- [fromY .. toY]]
  where
    side = unSideLength n
    b = boxLength n
    fromX = (x `div` b) * b
    fromY = (y `div` b) * b
    toX = min (side - 1) (fromX + b - 1)
    toY = min (side - 1) (fromY + b - 1)
