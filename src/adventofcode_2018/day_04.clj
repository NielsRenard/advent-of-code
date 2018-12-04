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
    [date event]))

(defn sleep-duration "takes two temporals and returns the duration between them"
  [falls-asleep wakes-up]
  (t/duration falls-asleep wakes-up))

(defn time-asleep
  [one-night]
  (apply t/plus (map #(apply sleep-duration %) (partition 2 one-night))))

(defn solve-part-1
  [puz-in]
  -1)
