all: build
build:
	@dune build --profile release
clean:
	@dune clean
