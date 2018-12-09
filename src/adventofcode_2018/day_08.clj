(ns adventofcode-2018.day-08
  (:require [util :refer :all]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def input (->> (first (util/get-input))
                split-by-whitespace
                (map edn/read-string)))

(def example-input
  '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(defn solve-part-1
  [puz-in]
  -1
  )
