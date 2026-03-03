# Review Findings

This document tracks code review findings that are likely to be raised by senior reviewers in PRs.
It is focused on actionable engineering concerns (correctness, safety, maintainability), not style preferences.
Open findings should stay concise and implementation-oriented; resolved findings can be moved to a history section later if needed.

## Severity Legend

- `P0`: critical issue, should be fixed immediately (crash/data loss/security class)
- `P1`: high-priority correctness/safety issue, should be fixed before or with nearby feature work
- `P2`: medium-priority maintainability/reliability issue, should be fixed soon
- `P3`: low-priority cleanup/refinement, nice to improve when touching related code

## Open Findings

### RF-008

- `Severity`: `P1`
- `Location`: `src/Sudoku/Solver/PuzzleSolver.hs:47`
- `Summary`: `solveWith` assumes every `Progress` result strictly advances state; if a strategy returns `Progress` with an unchanged grid, recursion can become non-terminating.
- `Why It Matters`: strategy implementations are pluggable via `solveWith`, so a buggy/custom strategy can cause infinite loops at runtime.
- `Suggested Change`: guard recursion with a progress check (`updated /= grid`), treat no-op progress as `Stuck` or `Contradiction`, or add a bounded-iteration safety mechanism.

### RF-009

- `Severity`: `P3`
- `Location`: `src/Sudoku/Symbols.hs:40`
- `Summary`: `mkSymbol` checks membership with list `elem`, giving linear lookup each time.
- `Why It Matters`: symbol lookup is a core operation across parsing/building/solving paths; linear membership can become avoidable overhead as board size and call volume grow.
- `Suggested Change`: store/derive a set-based representation for membership checks (e.g., keep `Set Char` alongside ordered symbols) while preserving existing symbol order semantics for rendering/output.
