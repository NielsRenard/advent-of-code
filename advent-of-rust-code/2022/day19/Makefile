ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

filename ?= example

run-debug:
	@RUST_LOG=debug cargo run --release

test-debug:
	@RUST_LOG=debug cargo test
