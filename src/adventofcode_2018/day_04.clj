(ns adventofcode-2018.day-04
  (:require [util :refer :all]
            [clojure.string :as string]
            [java-time :as t]))

(def input (get-input))


(defn parse-date [line]
  (let [split (-> line
                  (string/replace "[" "")
                  (string/split #"]"))

        date  (->> split
                   first
                   (t/local-date-time "yyyy-MM-dd HH:mm"))
        event (string/trim (first (rest split)))]
    [(.toString date) event]))

(defn solve-part-1
  [puz-in]
  -1)
