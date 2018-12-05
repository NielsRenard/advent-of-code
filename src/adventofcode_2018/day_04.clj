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

(defn sort-chronologically
  [input]
  (->> input
       (map parse-date)
       sort))


(defn sleep-duration "takes two temporals and returns the duration between them"
  [falls-asleep wakes-up]
  (t/duration falls-asleep wakes-up))


(defn time-asleep
  "Takes a shift, calculates sleep duration."
  [shift]
  (let [start-shift-event (first shift)
        sleep-wake-events (map first (map reverse (second shift)))]
    (->> sleep-wake-events
         (partition 2)
         (map #(apply sleep-duration %))
         (apply t/plus))))


(defn split-by-shift
  "Takes a chronologically sorted seq of events and splits them by shift. "
  [chrono-sorted-seq]
  (let [first-shift-elf (first chrono-sorted)]
    (->> chrono-sorted-seq
         (map reverse)
         (partition-by #(not= \G (ffirst %)))
         (partition 2))))


(defn solve-part-1
  [puz-in]
  (->> input
       sort-chronologically
       split-by-shift
       (map time-asleep)
       )
  )
