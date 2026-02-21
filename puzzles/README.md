# Puzzles

This directory contains example Sudoku puzzles used for development,
testing, and documentation purposes.

The examples serve as:

-   Reference inputs for the parser
-   Test fixtures for solver validation
-   Sample puzzles for manual experimentation
-   Demonstrations of valid and semantically invalid puzzles

For the complete input format specification, please refer to [PUZZLE_FORMAT.md](../PUZZLE_FORMAT.md).

## Directory Structure

    puzzles/
      valid/
        4x4/
        9x9/
      invalid/

### valid/

Contains well-formed Sudoku puzzles that:

-   Conform to the documented format
-   Are structurally valid (square, supported box shape)
-   Have a unique solution
-   Match their declared symbol set
-   Are solvable by the current solver implementation

Puzzles are grouped by grid size (e.g. `4x4`, `9x9`).

Each file is self-contained and includes metadata headers such as:

-   `id`
-   `variant`
-   `size`
-   `symbols`
-   `difficulty`
-   `source`
-   `created`

### invalid/

Contains intentionally invalid Sudoku puzzles in the puzzle sense.

These files are meant as real-world examples of grids that may still look like valid puzzle input,
but are not valid Sudoku puzzles because they violate Sudoku constraints or puzzle requirements.

Typical invalid cases include:

- Contradictions (no solution exists)
    - duplicate symbols in a row/column/box
    - givens that immediately conflict
- Non-unique puzzles (multiple solutions exist)

This directory is not primarily for testing parser error handling — unit tests cover those cases.
Instead, it documents inputs that should parse, but must be rejected by solving/validation logic.

## Puzzle Provenance

At this stage of the project, no Haskell-based generator has been implemented yet.

All valid example puzzles were generated using a minimal, self-written
Python generator and are marked with:

    source: external-generator

The Python generator is not part of the repository and was used solely
to bootstrap initial test data.

In the future, puzzles will be regenerated using the native Haskell
generator once it is implemented.
