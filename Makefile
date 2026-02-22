.PHONY: format build lint test run clean check

format:
	find app src test -name "*.hs" -exec ormolu -i {} \;

build:
	cabal build

lint:
	hlint src test

test:
	cabal test

check: format build lint test

run:
	cabal run sudoku --  --print-partial-solution --headers --stats puzzles/valid/9x9/classic-9x9-easy-9a0f5232245e02c6.sdk

run-dir:
	cabal run sudoku -- --dir puzzles/valid --print-partial-solution --print=solution --stats

clean:
	cabal clean
