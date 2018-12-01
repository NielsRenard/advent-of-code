(ns adventofcode-2018.day-01
  (:require [util :refer :all]))

(def input (util/get-input))

(defn solve-part-1 [puz-in]
  (reduce + puz-in))

(defn solve-part-2 [puz-in]
  (solve-part-1 puz-in))
