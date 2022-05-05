all: build
build:
	@dune build src --profile release
clean:
	@dune clean
dev:
	@dune build --profile release
	@cp -f _build/default/example/example_fetch.bc.js example/example-fetch.js
