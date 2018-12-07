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

;;something something, keep recurring until current length and length of previous run are the same
(defn zzz
  [input cnt]
  (let [new-count (count (drop 1 input))]
    (if (= new-count cnt)
      input
      (remove-pair-from-string (drop 1 input) new-count))))


(comment
  ;;quick solution with mutating string
  (def mutablestring (atom input))

  ;;count the atom for first answer
  (def brute-force-part-1 (dotimes [n 490]
                            (count(for [cs char-pairs]
                                    (swap! mutablestring #(remove-pair-from-string % cs))))))
  (count @mutablestring)

  (count (swap! mutablestring #(clojure.string/replace % #"[tT]" "")))
  ;; 10244
  (count (swap! mutablestring #(clojure.string/replace % #"[uU]" "")))
  ;;-
  (count (swap! mutablestring #(clojure.string/replace % #"[vV]" "")))
  ;;4944 taking a guess. correct.
  )
