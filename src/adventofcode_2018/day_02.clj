(ns adventofcode-2018.day-02
  (:require [util :refer :all]))

(def input (util/get-input))

(defn solve-part-1
  [puz-in]
  (let [twos   (get-duplicates puz-in 2)
        threes (get-duplicates puz-in 3)]
    (* twos threes)))

(defn get-duplicates
  "returns how many duplicate n's there are in a collection"
  [col n] (->> col
               (map frequencies)
               (map vals)
               (map distinct)
               (map #(filter #{n} %))
               (filter not-empty)
               count))
