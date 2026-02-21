# Sudoku

A Sudoku solver and generator written in Haskell (work in progress, very
early stage).

This project is primarily a vehicle to learn Haskell more deeply, get
familiar with its ecosystem (Cabal, testing, linting, CI), and practice
building clean, well-tested, readable software in a strongly typed
functional language.

## Project Vision

### Goals

-   Build a Sudoku solver with an extensible architecture for solving
    strategies
-   Add a generator for valid Sudoku puzzles
-   Support square Sudokus of arbitrary size (e.g. 4×4, 9×9, 16×16, ...)
-   Support custom symbol sets (numbers, letters, alphanumeric, etc.)
-   Maintain good engineering hygiene: tests, linting, CI

This repository focuses on clarity, correctness, and long-term
maintainability over quick feature accumulation.

## Using the Project

> ⚠ The Haskell-based generator is not yet implemented.
> Example puzzles currently stem from a minimal external Python
> generator.

### Requirements

-   GHC (recommended via GHCup)
-   Cabal
-   (Optional) HLint

### Build

``` sh
make build
```

### Run

``` sh
make run
```

### Tests

``` sh
make test
```

## Puzzle Format

Sudoku input files follow a simple structured text format.

The full specification is documented here: [PUZZLE_FORMAT.md](PUZZLE_FORMAT.md)

Example puzzle files can be found in the [puzzles](puzzles) directory.

## Development

### Commit Convention

This project follows the Conventional Commits specification: https://www.conventionalcommits.org/

#### Types used in this repository

-   `feat` --- new functionality
-   `fix` --- bug fixes
-   `refactor` --- refactoring without changing behavior
-   `perf` --- performance improvements
-   `test` --- tests only
-   `docs` --- documentation only
-   `build` --- build system / dependency changes
-   `ci` --- CI/CD related changes
-   `chore` --- maintenance tasks (formatting, cleanup, etc.)

#### Scopes used in this repository

Scopes are intentionally kept small and stable.

-   `core` --- shared core domain code (grid, units, symbols, etc.)
-   `solver` --- solver implementation
-   `generator` --- generator implementation

Scopes are only used for domain-level changes.
All other commits (build, CI, documentation, repository configuration,
etc.) are intentionally left without a scope.

## Git Hooks (Commit Message Validation + Pre-Push Checks)

This repository uses simple local Git hooks to:

-   validate commit messages against the Conventional Commits format
    (`commit-msg`)
-   run tests before pushing (`pre-push`)

The hooks are optional, but recommended.

### Install (once per clone)

``` sh
chmod +x scripts/install-hooks.sh
./scripts/install-hooks.sh
```

## Makefile

A small `Makefile` is provided as a convenience wrapper around common
development tasks (such as building and running tests).

It is intentionally lightweight and simply delegates to `cabal` (and
`hlint`, see next section).

## Linting (HLint)

This project uses HLint to keep the code idiomatic and clean.

### Install

Using GHCup:

``` sh
ghcup install hlint
```

### Run locally

``` sh
make lint
```

This runs HLint on both `src/` and `test/`.
