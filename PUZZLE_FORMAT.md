# Sudoku Puzzle Input Format

This document describes the textual input format for Sudoku puzzles used
in this project.

## Overview

A puzzle file consists of:

1.  An optional header section
2.  A required grid body

The format is human-readable and intentionally simple.

## Header Section (Optional)

The header section consists of the longest prefix of lines that are either:

-   Empty (whitespace only), or
-   Valid header lines

### Header Syntax

    key: value

### Rules

-   Exactly `": "` (colon + space) separates key and value.
-   The key must:
    -   Not be empty
    -   Not start with `-`
    -   Contain only `[A-Za-z0-9-]`
-   Header keys are case-insensitive.
-   Duplicate keys (case-insensitive) are rejected.
-   The value must not be empty.

Example:

    symbols: 1234
    comment: simple 4x4 puzzle

Currently recognized header key:

-   `symbols` --- declares the allowed symbol set

Other headers are allowed but ignored.

## Symbols

Symbols may come from:

1.  The `symbols` header
2.  A function argument (internal API usage)
3.  Inference from the grid (if neither is provided)

### Rules

-   Symbols must be unique.
-   Symbols must not contain:
    -   Whitespace
    -   `.`
    -   `:`
-   The number of symbols must exactly match the grid side length.
-   A mismatch between declared and provided symbols results in an
    error.

### Unicode Rule

Each symbol must be exactly one Unicode code point (`Char` in
Haskell).

Allowed:
-   `あ`
-   `♠`

Not supported: - Multi-codepoint grapheme sequences such as `❤️`

Since the parser operates on `Char`, multi-codepoint sequences are interpreted as multiple characters. This typically results in:
-   Incorrect grid length → `MalformedGrid`
-   Or invalid symbol usage → `SymbolCountMismatch`

## Grid Body

The grid body begins after the header section.

All whitespace characters (including spaces, tabs, CRLF, and Unicode
whitespace) are removed before parsing.

Allowed characters in the grid:

-   `.`
-   Declared symbols

Any other character results in a parse error.

## Grid Requirements

After whitespace removal, the grid must satisfy:

-   Length \> 0
-   Length must be a perfect square (n²)
-   `n` must itself be a perfect square (for square box layout)

Supported examples:

-   4×4 (2×2 boxes)
-   9×9 (3×3 boxes)
-   16×16 (4×4 boxes)

Unsupported examples:

-   5×5
-   6×6 (non-square box layout)

## Examples

See the [puzzles](puzzles) directory for complete example puzzle files.
