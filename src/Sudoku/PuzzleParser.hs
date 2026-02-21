module Sudoku.PuzzleParser
  ( ParseError(..)
  , parsePuzzle
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace, toLower)
import Data.Maybe (catMaybes)
import qualified Data.Set as S

import Sudoku.Symbols
  ( Symbols
  , Symbol
  , mkSymbol
  , mkSymbols
  , symbolChar
  , symbolsList
  )

import Sudoku.Placements
  ( Placements
  )

import Sudoku.Grid
  ( Coordinate
  )

----------------------------------------------------------------------
-- * Public Types
----------------------------------------------------------------------

data ParseError
  = InvalidHeader
  | DuplicateHeader
  | DuplicateDeclaredSymbols
  | AmbiguousSymbols
  | InvalidSymbols
  | SymbolCountMismatch
  | MalformedGrid
  | UnsupportedGridShape
  deriving (Eq, Show)

----------------------------------------------------------------------
-- * Public API
----------------------------------------------------------------------

parsePuzzle :: Maybe Symbols -> String -> Either ParseError (Symbols, Placements)
parsePuzzle symbolsArg input = do
  headers <- collectHeaders headerLines
  _ <- ensureNoDuplicateHeaders headers

  let declaredSymbolsValue = lookup "symbols" headers
  sideLength <- validateGridAndGetSideLength gridPayload

  symbols <- resolveSymbols symbolsArg declaredSymbolsValue sideLength gridPayload
  validateSymbolCountMatchesGrid symbols sideLength

  placements <- payloadToPlacements symbols sideLength gridPayload
  pure (symbols, placements)
  where
    (headerLines, gridLines) = splitHeaderSection (lines input)
    gridPayload = concatMap (filter (not . isSpace)) gridLines

----------------------------------------------------------------------
-- * Internal Helpers
----------------------------------------------------------------------

type HeaderKey   = String
type HeaderValue = String

splitHeaderSection :: [String] -> ([String], [String])
splitHeaderSection = span (\ln -> all isSpace ln || looksLikeHeaderLine ln)

looksLikeHeaderLine :: String -> Bool
looksLikeHeaderLine line =
  case break (== ':') line of
    (key, ':':_) -> not (null key) && all looksLikeHeaderKeyChar key
    _            -> False
  where
    looksLikeHeaderKeyChar ch = isHeaderKeyChar ch || ch `elem` "_/."

collectHeaders :: [String] -> Either ParseError [(HeaderKey, HeaderValue)]
collectHeaders headerLines = traverse parseHeaderLine (filter (not . all isSpace) headerLines)

parseHeaderLine :: String -> Either ParseError (HeaderKey, HeaderValue)
parseHeaderLine line =
  case break (== ':') line of
    (key, ':':' ':value)
      | null key                      -> Left InvalidHeader
      | head key == '-'                -> Left InvalidHeader
      | not (all isHeaderKeyChar key) -> Left InvalidHeader
      | null value                    -> Left InvalidHeader
      | otherwise                     -> Right (map toLower key, value)
    _                                 -> Left InvalidHeader

isHeaderKeyChar :: Char -> Bool
isHeaderKeyChar ch = isAsciiLower ch || isAsciiUpper ch || isDigit ch || ch == '-'

ensureNoDuplicateHeaders :: [(HeaderKey, HeaderValue)] -> Either ParseError [(HeaderKey, HeaderValue)]
ensureNoDuplicateHeaders headers
  | length keys == S.size (S.fromList keys) = Right headers
  | otherwise                               = Left DuplicateHeader
  where
    keys = [ k | (k, _) <- headers ]

resolveSymbols :: Maybe Symbols -> Maybe String -> Int -> String -> Either ParseError Symbols
resolveSymbols argSymbols headerSymbolsValue sideLength payload =
  case (argSymbols, headerSymbolsValue) of
    (Just sArg, Nothing)        -> do
      validateSymbols sArg
      Right sArg

    (Just sArg, Just rawHeader) -> do
      validateSymbols sArg
      sHeader <- parseDeclaredSymbols rawHeader
      if symbolsList sArg == symbolsList sHeader
        then Right sArg
        else Left AmbiguousSymbols

    (Nothing, Just rawHeader)   ->
      parseDeclaredSymbols rawHeader

    (Nothing, Nothing)          ->
      inferSymbolsFromPayload sideLength payload

parseDeclaredSymbols :: String -> Either ParseError Symbols
parseDeclaredSymbols raw = do
  validateDeclaredSymbolsChars raw
  case mkSymbols raw of
    Nothing -> Left DuplicateDeclaredSymbols
    Just s  -> Right s

validateDeclaredSymbolsChars :: String -> Either ParseError ()
validateDeclaredSymbolsChars xs
  | null xs        = Left InvalidSymbols
  | any invalid xs = Left InvalidSymbols
  | otherwise      = Right ()
  where
    invalid c = isSpace c || c == '.' || c == ':'

validateSymbols :: Symbols -> Either ParseError ()
validateSymbols s
  | any invalid (symbolsList s) = Left InvalidSymbols
  | otherwise                   = Right ()
  where
    invalid sym =
      let c = symbolChar sym
      in isSpace c || c == '.' || c == ':'

inferSymbolsFromPayload :: Int -> String -> Either ParseError Symbols
inferSymbolsFromPayload sideLength payload =
  case mkSymbols syms of
    Nothing -> Left InvalidSymbols
    Just s  -> Right s
  where
    syms = S.toList . S.fromList $ [ c | c <- payload, c /= '.', not (isSpace c) ]

validateGridAndGetSideLength :: String -> Either ParseError Int
validateGridAndGetSideLength payload
  | len == 0     = Left MalformedGrid
  | n * n /= len = Left MalformedGrid
  | b * b /= n   = Left UnsupportedGridShape
  | otherwise    = Right n
  where
    len = length payload
    n   = isqrt len
    b   = isqrt n
    isqrt x = floor (sqrt (fromIntegral x :: Double))

validateSymbolCountMatchesGrid :: Symbols -> Int -> Either ParseError ()
validateSymbolCountMatchesGrid symbols sideLength
  | length (symbolsList symbols) == sideLength = Right ()
  | otherwise                                  = Left SymbolCountMismatch

payloadToPlacements :: Symbols -> Int -> String -> Either ParseError Placements
payloadToPlacements symbols n payload =
  catMaybes <$> traverse mkPlacement (zip [0..] payload)
  where
    mkPlacement (_, '.') = Right Nothing
    mkPlacement (i, ch) =
      case mkSymbol symbols ch of
        Nothing     -> Left MalformedGrid
        Just symbol ->
          Right (Just ((i `mod` n, i `div` n), symbol))
