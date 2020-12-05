ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

in-java:
	 @echo "Java (2019):\n" && find ./advent-of-java-code/src/main/java -name Day**.java | sort | sed 's/[^0-9\/]*//g' | cut -c 7- | uniq --group -w 4

in-clojure: in-java
	 @echo "\nClojure(2017, 2018, (2019)):\n" && find ./advent-of-clojure-code/src -name day**.clj | sort | sed 's/[^0-9\/]*//g' | cut -c 4- | uniq --group -w 4

in-haskell: in-clojure
	 @echo "\nHaskell (2016, 2018, 2019, 2020):\n" && find ./advent-of-haskell-code/src -name Day**.hs | sort | sed 's/[^0-9\/]*//g' | cut -c 4- | uniq --group -w 4

solved: in-haskell

