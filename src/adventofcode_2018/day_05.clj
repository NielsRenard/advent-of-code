(ns adventofcode-2018.day-05
  (:require [util :refer :all]))

(def input (first (util/get-input)))

(def exin "dabAcCaCBAcCcaDA")

(defn react?
  "checks if two elements are the same type,
  and opposite polarity."
  [[c1 c2]]
  (= 32 (Math/abs (- (int c1)
                     (int c2)))))

;;doesn't work
(comment (defn solve-part-1
           ([input]
            (solve-part-1 input 0 ))
           ([input cursor]
            (let [char-pair   ((juxt first second) (drop cursor input))
                  new-cursor  (inc cursor)]
              (if (and (< new-cursor (count input))
                       (< new-cursor 5500))
                (if (react? char-pair)
                  (recur (clojure.string/replace input (apply str char-pair) "") 0)
                  (recur input new-cursor))
                input)))))


(def lower (seq "abcdefghijklmnopqrstuvwxyz"))
(def upper (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def char-pairs (partition 2 (interleave lower upper upper lower)))

(defn remove-pair-from-string
  [string pair]
  (clojure.string/replace string (apply str pair) ""))

;;n n'
(defn zzz
  [input cnt]
  (let [new-count (count (drop 1 input))]
    (if (= new-count cnt)
      input
      (remove-pair-from-string (drop 1 input) new-count))))



(def mutablestring (atom input))

(def brute-force-answer (dotimes [n 490]
                          (for [cs char-pairs]
                            (swap! mutablestring #(remove-pair-from-string % cs)))))
