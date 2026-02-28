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

### RF-001

- `Severity`: `P1`
- `Location`: `src/Sudoku/Geometry.hs:56`
- `Summary`: `SideLength (..)` is exported, allowing invalid values that can trigger division by zero in `boxOf`.
- `Why It Matters`: external code can construct `SideLength 0` (or negative), then `x \`div\` b` in `boxOf` can fail at runtime when `b == 0`.
- `Suggested Change`: make invalid side lengths unrepresentable (hide constructor and add smart constructor), or add explicit validation/guards in public APIs using `SideLength`.

### RF-002

- `Severity`: `P2`
- `Location`: `src/Sudoku/Geometry.hs:44`
- `Summary`: `boxLength` uses floating-point `sqrt` and `floor` for integral geometry logic.
- `Why It Matters`: this can silently accept non-perfect-square side lengths and introduces avoidable numeric fragility in core domain math.
- `Suggested Change`: validate with integer arithmetic (`b*b == side`) and define explicit behavior for invalid side lengths (e.g., `Maybe`, `Either`, or a validated constructor).

### RF-003

- `Severity`: `P1`
- `Location`: `src/Sudoku/Grid.hs:101`
- `Summary`: `setCell` does not validate that the input `Symbol` belongs to the grid's allowed symbol set.
- `Why It Matters`: callers can pass a `Symbol` created from a different `Symbols` set; this can introduce invalid fixed values into a grid and break solver/puzzle invariants.
- `Suggested Change`: enforce membership before setting (`symbol \`elem\` allowedSymbols grid` or set-based check), and return an explicit error for disallowed symbols.

### RF-004

- `Severity`: `P2`
- `Location`: `src/Sudoku/Grid.hs:114`
- `Summary`: `isComplete` uses an internal `error` via `requireCellAt` for a case that is expected to be unreachable.
- `Why It Matters`: partial code paths increase maintenance risk; future refactors can accidentally make the "unreachable" path reachable and fail at runtime.
- `Suggested Change`: implement completeness check directly over `cells` (e.g., `all isFixed (A.elems (cells grid))`) to remove the partial path entirely.

### RF-005

- `Severity`: `P2`
- `Location`: `src/Sudoku/PuzzleBuilder.hs:29`
- `Summary`: `buildPuzzle` uses `error` when `emptyGrid` returns `Nothing`.
- `Why It Matters`: this introduces a partial runtime failure path in public API code and relies on a cross-module invariant being permanently true.
- `Suggested Change`: make failure explicit in the type (e.g., return `Either BuildError Grid` with a constructor for invalid symbol count/shape), or constrain `Symbols` so invalid sizes are unrepresentable before `buildPuzzle`.

### RF-006

- `Severity`: `P2`
- `Location`: `src/Sudoku/PuzzleParser.hs:167`
- `Summary`: `validateGridAndGetSideLength` computes integer roots via floating-point `sqrt` + `floor`.
- `Why It Matters`: using floating-point for integral validation logic is fragile and can hide invalid cases behind rounding behavior.
- `Suggested Change`: use integer-safe validation for square checks (e.g., derive integer `n` and verify `n*n == len`, then `b*b == n`) without relying on floating-point root calculations.

### RF-007

- `Severity`: `P3`
- `Location`: `src/Sudoku/Solver/HiddenSingleStrategy.hs:49`
- `Summary`: `CountResult` carries `Ambiguous Int`, but the `Int` payload is never consumed.
- `Why It Matters`: unused payloads add noise, can trigger warnings, and obscure the true domain intent (`exactly one` vs `not exactly one`).
- `Suggested Change`: simplify to a boolean-style result (e.g., `Unique Placement | NotUnique`) or avoid the custom type and return `Maybe Placement` directly from `countCandidateCells`.

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
