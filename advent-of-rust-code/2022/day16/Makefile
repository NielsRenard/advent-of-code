ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

filename ?= example

run:
	@cargo run --release
	@dot  -Tsvg "graph.dot" -o "graph.svg"
	@xdg-open "graph.svg"


test:
	@cargo test -- --nocapture || true
	@dot  -Tsvg "graph.dot" -o "graph.svg"
	@xdg-open "graph.svg"
