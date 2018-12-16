(ns adventofcode-2018.day-11
  (:require [util :refer :all]
            [clojure.edn :as edn]))

(def input 3999)

(defn find-power-level
  [[serial x y]]
  (let [rack-id      (+ x 10)
        power-level (->> rack-id
                         (* y)
                         (+ serial)
                         (* rack-id)
                         str
                         (take-last 3)
                         first
                         str
                         edn/read-string
                         )]
    (- power-level 5)))

(defn grid-maker
  [serial]
  (for [x (range 300)
        y (range 300)]
    (find-power-level [serial x y])))

(defn solve-part-1
  [puz-in]
  -1
  )
