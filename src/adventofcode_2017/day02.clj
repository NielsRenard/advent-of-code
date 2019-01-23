(ns adventofcode-2017.day02
  (:require [adventofcode-2017.utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(def puz-in (utils/read-all-lines "resources/2017/02.txt"))

(defn sanitize-matrix [s]
  (->> s
       (map utils/split-on-tabs)
       (map utils/string->ints)))

(defn diff-min-and-max [seq]
  (-
   (apply max seq)
   (apply min seq)))

(defn sum-of-diffs [matrix]
  (reduce + (map diff-min-and-max matrix)))

(def solution (sum-of-diffs (sanitize-matrix puz-in)))

(defn divisable?
  ([[a b]]
   (divisable? a b))
  ([a b]
   (let [small (min a b)
         big   (max a b)]
     (== (int (/ big small )) (/ big small)))))



(defn find-divisable
  "find the one combination that is divisable, then divide"
  [coll]
  (first (filter divisable? (combo/combinations coll 2))))

(defn divide-pair
  [pair]
  (/ (apply max pair) (apply min pair)))

(defn solve [matrix]
  (->> matrix
       (map find-divisable)
       (map divide-pair)
       (reduce +)))

(def solution-2 (solve (sanitize-matrix puz-in)))
