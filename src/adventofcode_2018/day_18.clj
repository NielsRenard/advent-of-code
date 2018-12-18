(ns adventofcode-2018.day-18
  (:require [util :refer :all]
            [clojure.string :as string]))

(def input (util/get-input))

(def example-input
  ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.\n")

(defn parse-area [input]
  (->> input
       (map seq)))

(defn run-one-step [area]
  -1)
