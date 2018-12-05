(ns adventofcode-2018.day-04
  (:require [util :refer :all]
            [clojure.string :as string]
            [java-time :as t]
            [clojure.edn :as edn]))

(def input (get-input))



(defn parse-date [line]
  "parses the date to a java-time local-date object"
  (let [split (-> line
                  (string/replace "[" "")
                  (string/split #"]"))
        date  (->> split
                   first
                   (t/local-date-time "yyyy-MM-dd HH:mm"))
        event (string/trim (first (rest split)))]
    [date event]))

(defn new-parse-date [line]
  "parses the date to a java-time local-date object"
  (let [split (-> line
                  (string/replace "[" "")
                  (string/replace "#" "")
                  (string/split #"]"))
        date  (->> split
                   first
                   (t/local-date-time "yyyy-MM-dd HH:mm"))
        event (-> split
                  second
                  split-by-whitespace
                  (nth 2)
                  (edn/read-string)
                  )]
    [date event]))

(defn sort-chronologically
  [input]
  (->> input
       (map new-parse-date)
       sort))


(defn sleep-duration
  "takes two temporals and returns the duration between them"
  [falls-asleep wakes-up]
  (t/duration falls-asleep wakes-up))

(defn sleep-duration-with-starting
  "takes two temporals and returns the duration between them"
  [falls-asleep wakes-up]
  (hash-map :duration (-> (t/duration falls-asleep wakes-up)
                          .toString
                          (string/replace "PT" "")
                          (string/replace "M" "")
                          (Integer/parseInt)
                          )
            :started-at (->> falls-asleep
                             .toString
                             (drop-while #(not= \: %))
                             (drop 1)
                             (apply str)
                             (Integer/parseInt)
                             )))


(defn time-asleep
  "Takes a shift, calculates sleep duration."
  [shift]
  (let [start-shift-event (first shift)
        sleep-wake-events (map first (map reverse (second shift)))]
    {:who start-shift-event
     :sleep(->> sleep-wake-events
                (partition 2)
                (map #(apply sleep-duration %))
                (apply t/plus))}))


(defn new-time-asleep
  "Takes a shift, calculates sleep duration."
  [shift]
  (let [start-shift-event (first shift)
        sleep-wake-events (map first (map reverse (second shift)))]
    {:who start-shift-event
     :sleep(->> sleep-wake-events
                (partition 2)
                (map #(apply sleep-duration-with-starting %))
                )}))


(defn split-by-shift
  "Takes a chronologically sorted seq of events and splits them by shift. "
  [chrono-sorted-seq]
  (let [first-shift-elf (first chrono-sorted)]
    (->> chrono-sorted-seq
         (map reverse)
         (partition-by #(number? (first %)))
         (partition 2))))

(defn find-all-shifts-of-guard
  "gets all shifts of a certain elf id"
  [id]
  (filter #(= id (first (ffirst %))) (-> input
                                         sort-chronologically
                                         split-by-shift)))


(defn most-slept-minute
  "takes a seq of these kind of maps
  ({:started-at 42, :duration 16}"
  [durations]
  (map #(range (:started-at %) (inc (+ (:duration %)
                                       (:started-at %))))
       (map first durations)))

(def all-guard-ids (keys (group-by #(first (ffirst %)) split-shifts)))

(defn solve-part-1
  [puz-in]
  (->> puz-in
       sort-chronologically ;; parse-and-sort
       split-by-shift       ;;
       (map time-asleep)    ;; calc all sleep
       ))   ;; sort by longest sleep (it's guard 2137)

(comment
  ;;let's do a binary search
  ;;64110 too low (2137 * 30)
  ;;96165 too low 37 * 45)
  ;;81206 too high (2137 * 38)
  ;;74795 wrong * 35 (no high or low anymore)

  ;;that one elfs shifts
  (def sleepy-shifts (filter #(= 2137 (first (ffirst %))) (-> input
                                                              sort-chronologically
                                                              split-by-shift)))

  (def durations
    (->> sleepy-shifts
         (map new-time-asleep)
         (map second)
         (map second)))

  (frequencies (flatten (most-slept-minute durations)))

  ;;that one guys total sleep times
  (filter #(= 2137 ((comp ffirst :who) %)) here
          ))
