(ns adventofcode-2017.day05
  (:require [adventofcode-2017.utils :as utils]))


(def puz-in (into [] (utils/read-all-lines-to-int "resources/2017/05.txt")))


(def example-input [0
                    3
                    0
                    1
                    -3])


(defn- calc-jump
  "Calculates the offset to jump by."
  [curs seq]
  (nth seq curs))


(defn- inc-cursor-value
  "Increases the value of the value under the cursor (pre-jump)."
  [curs seq]
  (assoc seq curs (inc (nth seq curs))))


(defn better-step-counter
  "Step counter, returns steps needed to step out of bounds."
  [curs steps seq]
  (let [new-curs (+ curs (calc-jump curs seq))]
    (if (or (<= 0 new-curs (- (count seq) 1)))
      (recur new-curs
             (inc steps)
             (inc-cursor-value curs seq))
      (+ 1 steps))))


(defn- dec-cursor-value
  "Decreases the value of what's under the cursor. TODO: rewrite this"
  [curs seq]
  (let [val (nth seq curs)]
    (assoc seq curs (if (zero? val)
                      (inc val)
                      (if (pos? val)
                        (dec val)
                        (inc val))))))

(defn- jump-less-than-3?
  "Returns true if the jump offset is two steps or less"
  [curs new-curs]
  (<= -2 (- curs new-curs) 2))

(defn- cursor-not-out-of-bounds?
  "Returns true if index within bounds of arrays"
  [curs seq]
  (<= 0 curs (- (count seq) 1)))


(defn part-two-step-counter
  "Step counter for part two, returns # steps to step out of bounds"
  [curs steps seq]
  (let [new-curs (+ curs (calc-jump curs seq))]
    (if (cursor-not-out-of-bounds? new-curs seq)
      (recur new-curs
             (inc steps)
             (if (jump-less-than-3? curs new-curs)
               (inc-cursor-value curs seq)
               (dec-cursor-value curs seq)))
      (inc steps))))


#_(part-two-step-counter 0 0 example-input)
;;(def solution-A (part-two-step-counter 0 0 puz-in))
