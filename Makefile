.PHONY: build test clean doc

build:
	dune build @install

test:
	dune runtest

clean:
	dune clean

doc:
	dune build @doc

.DEFAULT_GOAL := build
