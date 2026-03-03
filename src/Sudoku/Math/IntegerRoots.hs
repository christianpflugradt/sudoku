module Sudoku.Math.IntegerRoots (perfectSquareRoot) where

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

perfectSquareRoot :: Int -> Maybe Int
perfectSquareRoot x
  | x <= 0 = Nothing
  | root * root == x = Just root
  | otherwise = Nothing
  where
    root = isqrtFloor x

----------------------------------------------------------------------
-- Internal Helpers
----------------------------------------------------------------------

isqrtFloor :: Int -> Int
isqrtFloor x
  | x <= 0 = 0
  | otherwise = go 1 x
  where
    go lo hi
      | lo > hi = hi
      | mid <= x `div` mid = go (mid + 1) hi
      | otherwise = go lo (mid - 1)
      where
        mid = lo + (hi - lo) `div` 2
