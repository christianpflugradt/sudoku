.PHONY: build test run clean check

build:
	cabal build

test:
	cabal test

run:
	cabal run sudoku

clean:
	cabal clean

check: build test