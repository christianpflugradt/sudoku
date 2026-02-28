module Sudoku.App.Render
  ( renderGrid,
    renderGridCompact,
  )
where

import Data.List (intercalate)
import Sudoku.Grid
  ( Cell (..),
    Grid,
    boundsOf,
    cellAt,
    sideLength,
  )
import Sudoku.Symbols (symbolChar)

renderGrid :: Grid -> String
renderGrid grid =
  unlines (renderPretty grid)

renderGridCompact :: Grid -> String
renderGridCompact grid =
  unlines (renderRows grid)

renderPretty :: Grid -> [String]
renderPretty grid =
  concat (zipWith renderRowBlock [0 ..] rowBlocks)
  where
    n = sideLength grid
    b = floor (sqrt (fromIntegral n :: Double))
    rowBlocks = chunk b [0 .. n - 1]

    renderRowBlock :: Int -> [Int] -> [String]
    renderRowBlock idx ys =
      let rows = map (renderPrettyRow grid b) ys
          isLast = idx == length rowBlocks - 1
       in if isLast
            then rows
            else rows <> [horizontalSep b]

renderPrettyRow :: Grid -> Int -> Int -> String
renderPrettyRow grid boxSize y =
  intercalate " | " (map renderSegment (chunk boxSize [0 .. side - 1]))
  where
    side = boxSize * boxSize
    renderSegment xs = unwords [cellStr (x, y) | x <- xs]

    cellStr coord =
      case cellAt grid coord of
        Just (Fixed s) -> [symbolChar s]
        Just (Empty _) -> "."
        Nothing -> "?"

horizontalSep :: Int -> String
horizontalSep b =
  intercalate "-+-" (replicate b segment)
  where
    segment = replicate (2 * b - 1) '-'

renderRows :: Grid -> [String]
renderRows grid =
  [concat [cellChar (x, y) | x <- [0 .. upperX]] | y <- [0 .. upperY]]
  where
    ((_, _), (upperX, upperY)) = boundsOf grid
    cellChar coord =
      case cellAt grid coord of
        Just (Fixed s) -> [symbolChar s]
        Just (Empty _) -> "."
        Nothing -> "?"

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk k xs =
  let (h, t) = splitAt k xs
   in h : chunk k t
