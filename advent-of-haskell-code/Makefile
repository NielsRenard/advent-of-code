ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

format:
	@cd $(ROOT) && \
	hindent app/Main.hs && \
	stylish-haskell -i app/Main.hs && \
  find src -name '*.hs' -exec hindent {} \; && \
	find src -name '*.hs' | xargs stylish-haskell -i && \
  find test -name '*.hs' -exec hindent {} \;

exec:
	@stack exec advent-of-haskell-code-exe

run:exec
