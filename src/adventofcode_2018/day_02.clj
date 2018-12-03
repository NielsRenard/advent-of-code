(ns adventofcode-2018.day-02
  (:require [util :refer :all]
            [clojure.data :as data]))

(def input (util/get-input))


(defn get-duplicates
  "returns how many duplicate n's there are in a collection"
  [col n] (->> col
               (map frequencies)
               (map vals)
               (map distinct)
               (map #(filter #{n} %))
               (filter not-empty)
               count))

(defn solve-part-1
  [puz-in]
  (let [twos   (get-duplicates puz-in 2)
        threes (get-duplicates puz-in 3)]
    (* twos threes)))



;;TODO: optimize by halting the list comprehension when a value is found
(defn just-one-difference?
  "Returns the diff for strings with one character difference."
  [s1 s2]
  (let [diff   (data/diff (seq s1) (seq s2))
        length (count s1)]
    (if (->> diff
             last
             (remove nil?)
             count
             (- length)
             (= 1))
      (last diff))))

(defn solve-part-2
  [puz-in]
  (let [answer (for [s1 puz-in
                     s2 puz-in]
                 (just-one-difference? s1 s2))]
    (->> answer
         (remove nil?)
         first
         (remove nil?)
         (apply str))))
