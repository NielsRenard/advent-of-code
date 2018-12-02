(ns adventofcode-2018.day-01
  (:require [util :refer :all]
            [clojure.edn :as edn]))

(def input (->> (util/get-input)
                (map edn/read-string ,,,)))

(defn solve-part-1 [puz-in]
  (reduce + puz-in))

(defn solve-part-2 [puz-in]
  (solve-part-1 puz-in))
