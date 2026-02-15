# Sudoku

A Sudoku solver and generator written in Haskell (work in progress, very early stage).

This project is primarily a vehicle to learn Haskell more deeply, get familiar with its ecosystem (Cabal, testing, linting, CI), and practice building clean, well-tested, readable software in a strongly typed functional language.

## Goals

- Build a Sudoku solver with an extensible architecture for solving strategies
- Add a generator for valid Sudoku puzzles
- Support square Sudokus of arbitrary size (e.g. 4×4, 9×9, 16×16, …)
- Support custom symbol sets (numbers, letters, alphanumeric, etc.)
- Maintain good engineering hygiene: tests, linting, CI

## Commit Convention

This project follows the **Conventional Commits** specification:

- https://www.conventionalcommits.org/

### Types used in this repository

- `feat` — new functionality
- `fix` — bug fixes
- `refactor` — refactoring without changing behavior
- `perf` — performance improvements
- `test` — tests only
- `docs` — documentation only
- `build` — build system / dependency changes
- `ci` — CI/CD related changes
- `chore` — maintenance tasks (formatting, cleanup, etc.)

### Scopes used in this repository

Scopes are intentionally kept small and stable.

- `core` — shared core domain code (grid, units, symbols, etc.)
- `solver` — solver implementation
- `generator` — generator implementation

Scopes are only used for domain-level changes. All other commits (build, CI, documentation, repository configuration, etc.) are intentionally left without a scope.