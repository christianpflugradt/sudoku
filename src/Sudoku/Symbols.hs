module Sudoku.Symbols
  ( Symbols,
    mkSymbols,
    Symbol,
    mkSymbol,
    symbolChar,
    symbolsList,
  )
where

import qualified Data.Set as S

----------------------------------------------------------------------

-- * Public Types

----------------------------------------------------------------------

newtype Symbols = Symbols [Char] deriving (Eq, Ord, Show)

newtype Symbol = Symbol Char
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------

-- * Public API

----------------------------------------------------------------------

mkSymbols :: [Char] -> Maybe Symbols
mkSymbols [] = Nothing
mkSymbols xs
  | unique = Just (Symbols xs)
  | otherwise = Nothing
  where
    unique = length xs == S.size (S.fromList xs)

mkSymbol :: Symbols -> Char -> Maybe Symbol
mkSymbol (Symbols xs) c
  | c `elem` xs = Just (Symbol c)
  | otherwise = Nothing

symbolsList :: Symbols -> [Symbol]
symbolsList (Symbols xs) = map Symbol xs

symbolChar :: Symbol -> Char
symbolChar (Symbol c) = c
