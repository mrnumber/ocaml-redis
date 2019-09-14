
all: build test

build:
	@dune build @all

test:
	@dune runtest --force --no-buffer

clean:
	@dune clean

watch:
	@dune build @all -w

.PHONY: all build test clean watch
