(ns adventofcode-2018.day-06
  (:require [util :refer :all]
            [clojure.edn :as edn]))

(def input (util/get-input))

(defn clean [s]
  (->> (re-seq  #"\d+" s)
       (map edn/read-string)
       (zipmap [:x :y])))

(defn max-dimensions [puz-in]
  (let [cleaned (map clean puz-in)
        get-min (fn [a] (apply min (map a cleaned) ))
        get-max (fn [a] (apply max (map a cleaned) ))
        min-x   (get-min :x)
        max-x   (get-max :x)
        min-y   (get-min :y)
        max-y   (get-max :y)]
    {:x-min min-x, :x-max max-x
     :y-min min-y, :y-max max-y}))

;;(max-dimensions input)

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (let [h-dist (- x1 x2)
        v-dist (- y1 y2)]
    {:h-dist (if (neg? h-dist) (Math/abs h-dist) h-dist)
     :v-dist (if (neg? v-dist) (Math/abs v-dist) v-dist)}))

(defn calculate-all-distances
  [[x y] nodes]
  (for [node nodes]
    (manhattan-distance [x y] (vals node))))


(defn solve-part-1
  [puz-in]
  (->> puz-in
       (map clean)))
