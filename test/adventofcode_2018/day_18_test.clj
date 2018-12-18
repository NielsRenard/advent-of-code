(ns adventofcode-2018.day-18-test
  (:require [adventofcode-2018.day-18 :as sut]
            [clojure.test :refer [is are deftest]]
            [clojure.string :as string]))


(def example-input
  (string/split-lines ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.\n"))


(deftest run-one-step-test
  (are [input expected] (= expected (sut/run-one-step input))

    ;;initial state
    ".#.#...|#.
    .....#|##|
    .|..|...#.
    ..|#.....#
    #.#|||#|#|
    ...#.||...
    .|....|...
    ||...#|.#|
    |.||||..|.
    ...#.|..|."

    ;;after 1 minute

    ".......##.
    ......|###
    .|..|...#.
    ..|#||...#
    ..##||.|#|
    ...#||||..
    ||...|||..
    |||||.||.|
    ||||||||||
    ....||..|."))
