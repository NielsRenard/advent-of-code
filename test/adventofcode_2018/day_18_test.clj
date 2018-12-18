(ns adventofcode-2018.day-18-test
  (:require [adventofcode-2018.day-18 :as sut]
            [clojure.test :refer [is are deftest]]))

(def example-input
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
  )

(deftest
  (are [input expected] (= expected (sut/run-one-step input))
    1 -1))
