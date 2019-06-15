(ns adventofcode-2018.day-14
  (:require [util :refer :all]))

(def input 440231)

(defn generate-recipes [puz-in elf1-pos elf2-pos n]
  (let [
        recipe-1-score (get puz-in elf1-pos)
        recipe-2-score (get puz-in elf2-pos)
        new-recipes    (->> (+ recipe-1-score recipe-2-score)
                            str
                            (map #(Character/getNumericValue %))
                            (into puz-in))
        l              (count new-recipes)
        e1-new-pos     (mod (+ elf1-pos (inc recipe-1-score)) l) ;; prevent idx OOB
        e2-new-pos     (mod (+ elf2-pos (inc recipe-2-score)) l)
        ]
    (if (< n 400000) ;; how high should this be?
      (recur new-recipes
             e1-new-pos
             e2-new-pos
             (inc n))
      new-recipes)))

(defn solve-part-1 [input]
  ;; time (solve-part-1 input))
  ;; "Elapsed time: 1985.910501 msecs"
  ;; "1052903161"
  (->> (generate-recipes [3 7] 0 1 0)
       (drop input)
       (take 10)
       (apply str)
       ))
