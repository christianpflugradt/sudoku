# Review Findings

This document tracks code review findings that are likely to be raised by senior reviewers in PRs.
It is focused on actionable engineering concerns (correctness, safety, maintainability), not style preferences.
Open findings should stay concise and implementation-oriented; resolved findings can be moved to a history section later if needed.

## Severity Legend

- `P0`: critical issue, should be fixed immediately (crash/data loss/security class)
- `P1`: high-priority correctness/safety issue, should be fixed before or with nearby feature work
- `P2`: medium-priority maintainability/reliability issue, should be fixed soon
- `P3`: low-priority cleanup/refinement, nice to improve when touching related code

## Review Scope And Criteria

When running a code review pass for findings in this document:

- `Scope (current default)`: review only modules under `src/Sudoku` excluding `src/Sudoku/App`.
- `Excluded`: `app/`, `src/Sudoku/App/`, and `test/`.
- `Finding bar`: only include issues that are concrete improvements under `P0`-`P3` (correctness, safety, reliability, maintainability).
- `Do not include`: style-only nits, personal preference, or "possible alternative" suggestions without clear engineering benefit.
- `No-findings outcome`: it is valid to report no findings when nothing meets the bar.
- `Reporting format`: list findings ordered by severity with precise file/line references.

## Open Findings

No open findings at this time.

## Resolved Findings

### RF-010

- `Severity`: `P3`
- `Location`: `src/Sudoku/Geometry.hs:57`
- `Summary`: `boxLength` used a partial `error` branch for an "unreachable" invariant violation.
- `Resolution`: `SideLength` now stores both validated side length and box length at construction time, and `boxLength` is a total field access with no runtime error path.

## Deferred By Design

### RF-009

- `Severity`: `P3`
- `Location`: `src/Sudoku/Symbols.hs:40`
- `Summary`: `mkSymbol` checks membership with list `elem`, giving linear lookup each time.
- `Why It Matters`: symbol lookup is used across parsing/building/solving paths; linear membership can become avoidable overhead as board size and call volume grow.
- `Suggested Change`: store/derive a set-based representation for membership checks (e.g., keep `Set Char` alongside ordered symbols) while preserving existing symbol order semantics for rendering/output.
- `Status`: Deferred by design
- `Reason`: symbol-set sizes in this project are small (typically 9, up to ~25 for human-solvable variants), so this is not expected to be a meaningful bottleneck relative to other solver costs.
- `Revisit Trigger`: revisit only if profiling shows symbol membership is hot, or if much larger grid sizes become a target.
