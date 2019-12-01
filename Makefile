ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

solved:
	 @find ./advent-of-haskell-code -name Day**.hs | sort | sed 's/[^0-9\/]*//g' | cut -c 4- | xargs echo -e 'Haskell:\n' \
		&& find -name day**.clj | sort | sed 's/[^0-9\/]*//g' | cut -c 4- | xargs echo -e 'Clojure:\n'
