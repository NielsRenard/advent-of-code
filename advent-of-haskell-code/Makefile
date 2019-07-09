ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

format:
	@cd $(ROOT) && \
	hindent app/Main.hs && \
	stylish-haskell -i app/Main.hs && \
	find src -name '*.hs' -print | xargs brittany --write-mode=inplace && \
	find test -name '*.hs' -print | xargs brittany --write-mode=inplace
exec:
	@stack exec advent-of-haskell-code-exe

run:exec
