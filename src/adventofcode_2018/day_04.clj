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
       (map parse-date)
       sort))


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
         (partition-by #(number? (last %)))
         (partition 2))))

(defn get-all-shifts-of-guard
  "gets all shifts of a certain elf id"
  [puz-in id]
  (filter #(= id (second (ffirst %))) (-> puz-in
                                          sort-chronologically
                                          split-by-shift)))

;;could probably be a lot more effient with a regex
;;using the split-by-shift output makes this slow
(defn get-all-guard-ids
  "returns the ids of all the guards, sorted low to high"
  [puz-in]
  (let [sorted-input (-> puz-in
                         sort-chronologically
                         split-by-shift)
        get-id-fn    (fn [line]
                       (second (ffirst line)))]
    (->> sorted-input
         (group-by get-id-fn)
         keys
         sort)))


(defn get-shift-durations
  [shifts]
  (->> shifts
       (map time-asleep)
       (map second)
       (map second)))


(defn most-slept-minute
  "calculates all the minutes a guard was asleep, and
  returns the minute that is most frequent."
  [durations]
  (->> durations
       (map #(range (:started-at %) (+ (:duration %)
                                       (:started-at %))))
       flatten
       frequencies
       (sort-by val)
       last))


(defn find-most-slept-minute-of-guard
  "returns the minute the guard was most frequently asleep (and how many times this occured)."
  [puz-in id] (most-slept-minute (flatten (get-shift-durations (get-all-shifts-of-guard puz-in id)))))


(defn total-slept-minutes [puz-in id]
  "calculates how many minutes this elf slept"
  (reduce + (flatten (map #(map :duration %) (get-shift-durations (get-all-shifts-of-guard puz-in id))))))


(defn solve-part-1
  "Strategy 1: Find the guard that has the most minutes asleep.
   What minute does that guard spend asleep the most?"
  [puz-in]
  (let [all-guard-ids      (get-all-guard-ids puz-in)
        most-sleepy-guard  (->> (map #(total-slept-minutes puz-in %) all-guard-ids)
                                (#(zipmap % all-guard-ids))
                                (apply max-key key)
                                val)
        most-sleepy-minute (key (find-most-slept-minute-of-guard puz-in most-sleepy-guard))]
    (* most-sleepy-guard
       most-sleepy-minute)))

(defn solve-part-2
  "Strategy 2: Of all guards, which guard
  is most frequently asleep on the same minute?"
  [puz-in]
  (let [all-guard-ids (get-all-guard-ids puz-in)]

    (->> (map  #(find-most-slept-minute-of-guard puz-in %) all-guard-ids)
         (zipmap all-guard-ids)
         (sort-by (comp val val))
         last
         flatten
         drop-last
         (reduce *))))
