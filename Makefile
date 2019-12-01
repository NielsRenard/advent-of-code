ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

in-haskell:
	 @echo "Haskell:\n" && find ./advent-of-haskell-code/src -name Day**.hs | sort | sed 's/[^0-9\/]*//g' | cut -c 4- | uniq --group -w 4

in-clojure: in-haskell
	 @echo "\nClojure:\n" && find ./advent-of-clojure-code/src -name day**.clj | sort | sed 's/[^0-9\/]*//g' | cut -c 4- | uniq --group -w 4

solved: in-clojure

