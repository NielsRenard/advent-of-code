(ns adventofcode-2018.day-14
  (:require [util :refer :all]))

(def input 440231)

(defn solve-part-1 [puz-in elf1-pos elf2-pos]
  (let [row        (seq puz-in)
        elf1-steps (inc (nth row elf1-pos))
        elf2-steps (inc (nth row elf2-pos))]
    (println (str "row" row "," "elf1-steps" elf1-steps ", elf2-steps" elf2-steps "."))))
