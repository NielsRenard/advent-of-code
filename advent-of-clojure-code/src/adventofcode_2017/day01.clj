(ns adventofcode-2017.day01
  (:require [util :as util]))

(def puz-input (util/read-all-lines-to-string "resources/2017/01.txt"))

(defn string-to-numbers [s]
  (map #(Character/getNumericValue %) s))

(def numseq (string-to-numbers puz-input))

(defn matching-pair? [pair]
  (= (first pair)
     (second pair)))

(defn solution
  "Takes a sequence of numbers and returns the solution"
  ([numseq]
   (if (matching-pair? [(first numseq) (last numseq)])
     (solution numseq (first numseq))
     (solution numseq 0)))

  ([numseq aggr]
   (let [first (first numseq)
         rest (rest numseq)
         pair (take 2 numseq)]

     (if (nil? (next numseq))
       aggr
       (if (matching-pair? pair)
         (recur rest (+ aggr first))
         (recur rest aggr))))))

(defn solve [] (solution numseq))

;; part two


;;part two
(defn halfway-finder
  "takes a seq, returns whats halfway the first digit"
  [numseq]
  (let [index-half (/ (count numseq) 2)]
    index-half))


(defn halfway-equal?
  [pos aggr seq]
  (let [looped-numseq (cycle seq)
        val1          (nth looped-numseq pos)
        halfway-step  (+ pos (/ (count seq) 2))]
    (if (>= pos (count seq))
      aggr
      (if (= val1 (nth looped-numseq halfway-step))
        (recur (inc pos) (+ aggr val1) seq)
        (recur (inc pos) aggr seq)))))

(comment (halfway-equal? 0 0 numseq))
