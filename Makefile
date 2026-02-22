.PHONY: format build lint test run clean check

format:
	find app src test -name "*.hs" -exec ormolu -i {} \;

build:
	cabal build

lint:
	hlint src test

test:
	cabal test

check: build lint test

run:
	cabal run sudoku

clean:
	cabal clean
