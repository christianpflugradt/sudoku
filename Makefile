.PHONY: build lint test run clean check

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
