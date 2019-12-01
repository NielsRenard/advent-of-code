ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

exec:
	@stack exec advent-of-haskell-code-exe

run:exec

ormolu:
	 @cd $(ROOT) && \
	 find app  -name '*.hs' -exec ormolu --mode inplace {} \; && \
	 find src  -name '*.hs' -exec ormolu --mode inplace {} \; && \
	 find test -name '*.hs' -exec ormolu --mode inplace {} \;

