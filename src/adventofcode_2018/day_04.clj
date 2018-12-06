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
                   (t/local-date-time "yyyy-MM-dd HH:mm")
                   .toString)
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
  (let [falls (t/local-date-time falls-asleep)
        wakes (t/local-date-time wakes-up)]
    (hash-map :duration (-> (t/duration falls wakes)
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
                               ))))


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
        sleep-wake-events (map second (map reverse (second shift)))]
    {:who start-shift-event
     :sleep(->> sleep-wake-events
                (partition 2)
                (map #(apply sleep-duration-with-starting %))
                )}))


(defn split-by-shift
  "Takes a chronologically sorted seq of events and splits them by shift. "
  [chrono-sorted-seq]
  (let [first-shift-elf (first chrono-sorted-seq)]
    (->> chrono-sorted-seq
         (map reverse)
         (partition-by #(number? (first %)))
         (partition 2))))

(+ 2137)

(defn new-split-by-shift
  "Takes a chronologically sorted seq of events and splits them by shift. "
  [chrono-sorted-seq]
  (let [first-shift-elf (first chrono-sorted-seq)]
    (->> chrono-sorted-seq
         (partition-by #(number? (last %)))
         (partition 2))))

(defn get-all-shifts-of-guard
  "gets all shifts of a certain elf id"
  [id]
  (filter #(= id (second (ffirst %))) (-> input
                                          sort-chronologically
                                          new-split-by-shift)))

;;TODO rewrite this so it reads better, and make it a function
(def all-guard-ids (->>
                    (-> input
                        sort-chronologically
                        new-split-by-shift)
                    (group-by #(second (ffirst %)))
                    ((comp sort keys))))

(defn get-shift-durations
  [shifts]
  (->> shifts
       (map new-time-asleep)
       (map second)
       (map second)))

(defn most-slept-minute
  "takes a seq of these kind of maps
  ({:started-at 42, :duration 16}"
  [durations]
  (->> durations
       (map #(range (:started-at %) (+ (:duration %)
                                       (:started-at %))))
       flatten
       frequencies
       (sort-by val)
       last
       first
       ))

(defn maaa
  "takes a seq of these kind of maps
  ({:started-at 42, :duration 16}"
  [durations]
  (->> durations
       (map #(range (:started-at %) (+ (:duration %)
                                       (:started-at %))))
       flatten
       frequencies

       ))

(defn baaa
  [id] (maaa (flatten (get-shift-durations (get-all-shifts-of-guard id)))))


(defn find-most-slept-minute-guard
  [id] (most-slept-minute (flatten (get-shift-durations (get-all-shifts-of-guard id)))))


(defn my-most-slept-minute
  [durations]
  (for [s (map #(map :started-at %) durations)
        d (map #(map :duration %) durations)]
    [(count s) (count d)]))

(defn total-slept-minutes [id]
  (reduce + (flatten (map #(map :duration %) (get-shift-durations (get-all-shifts-of-guard id))))))


(defn solve-part-1
  [puz-in]
  (->> puz-in
       sort-chronologically ;; parse-and-sort
       new-split-by-shift       ;;
       (map new-time-asleep)    ;; calc all sleep
       ))   ;; sort by longest sleep (it's guard 2137)
                                        ;


(comment

  ;; part two:
  ;; (map find-most-slept-minute all-guard-ids)
  ;; check which has the highest frequency of a number
  ;; poof thats it

  ;;let's do a binary search
  ;;64110 too low (2137 * 30)
  ;;96165 too low 37 * 45)
  ;;81206 too high (2137 * 38)
  ;;74795 wrong * 35 (no high or low anymore)

  ;;elf 1993 has most minutes asleep but I get multiple hits for most slept minute

  ;;that one elfs shifts
  (def sleepy-shifts (filter #(= 2137 (first (ffirst %))) (-> input
                                                              sort-chronologically
                                                              new-split-by-shift)))

  (def durations
    (->> sleepy-shifts
         (map new-time-asleep)
         (map second)
         (map second)))

  (def (sort-by val (frequencies (flatten (most-slept-minute (get-shift-durations (get-all-shifts-of-guard 2137))))))
    (frequencies (flatten (most-slept-minute durations))))

  ;;that one guys total sleep times
  (filter #(= 2137 ((comp ffirst :who) %)) here
          ))
