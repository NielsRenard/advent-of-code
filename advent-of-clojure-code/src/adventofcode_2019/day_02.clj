(ns adventofcode-2019.day-02
  (:require [util :refer :all]
            [clojure.edn :as edn]))

(def input (->> (util/get-input)
                (map edn/read-string)))

(defn calcFuel [mass]
  (- (Math/floor (int (/ mass 3))) 2))

(defn recursiveFuel [mass]
  (if (> (calcFuel mass) 0)
    (+ (calcFuel mass) (recursiveFuel (calcFuel mass)))
    0))

(defn solve-part-1 [puz-in]
  (reduce + (map calcFuel puz-in)))

(defn solve-part-2 [puz-in]
  (reduce + (map recursiveFuel puz-in)))
