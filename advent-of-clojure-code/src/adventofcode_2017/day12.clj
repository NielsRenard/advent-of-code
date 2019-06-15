(ns adventofcode-2017.day12
  (:require [clojure.string :as string :refer [split]]
            [adventofcode-2017.utils :as utils]))

(def puz-in(utils/read-all-lines "resources/2017/12.txt"))

(def ex-multi (nth puz-in 31))
(def ex-one (nth puz-in 30))


(defn string->vec [s]
  (-> s
      utils/remove-commas
      (split #" ")))

(defn vec->mapofsets [v]
  (hash-map (first v) (set (nthrest v 2))))

(defn serialize [inp]
  (-> inp
      string->vec
      vec->mapofsets))
