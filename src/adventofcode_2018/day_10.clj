(ns adventofcode-2018.day-10
  (:require [util :refer :all]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def input (util/get-input))

(defn parse-line
  "parses this string:
    position=< 9,  1> velocity=< 0,  2>
    {:x 9, :y 1, :dx 5, :dy 1}"
  [line]
  (let [split (-> line
                  (string/replace "position=<" "")
                  (string/replace ", " " ")
                  (string/replace "> velocity=<" " ")
                  (string/replace ", " " ")
                  (string/replace ">" "")
                  (string/split #" ")
                  (->>
                   (filter #(not (empty? %)))
                   (map edn/read-string)))]

    {:x  (first split) :y  (second split)
     :dx (nth split 2) :dy (last split)}))

(defn translate
  "Takes a pair of coordinates & and its velocity, calculates
   the next position. e.g.
   {:x 9,  :y 1, :dx 5, :dy 1} becomes
   {:x 14, :y 2, :dx 5, :dy 1}"
  [{:keys [x y dx dy]}]
  {:x (+ x dx) :y (+ y dy) :dx dx :dy dy})


(defn parse-data [puz-in]
  (->> puz-in
       (map parse-line)))


(defn solve-part-1
  [puz-in]
  -1
  )
