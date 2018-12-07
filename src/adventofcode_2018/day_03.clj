(ns adventofcode-2018.day-03
  (:require [util :refer :all]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def input (util/get-input))

;;committing the mess before cleanup to enjoy in git in the future

(defn sanitize-entry
  "parses a line like:
  `#1 @ 483,830: 24x18`
  into an entry `((483 830) (26 16))`."
  [line]
  (let []
    (partition 2
               (drop 1
                     (map #(edn/read-string %) ;;parse to ints
                          (-> line
                              (string/replace ":" "")
                              (string/replace "#" "")
                              (string/replace "@ " "")
                              (string/replace "," " ")
                              (string/replace "x" " ")
                              (string/split #" ")))))))

(defn nice-entry
  "parses a line like:
  `#1 @ 483,830: 24x18`"
  [line]
  (let [parsed (map #(edn/read-string %)
                    (-> line
                        (string/replace ":" "")
                        (string/replace "#" "")
                        (string/replace "@ " "")
                        (string/replace "," " ")
                        (string/replace "x" " ")
                        (string/split #" ")))]
    (seq [(seq [(second parsed) (nth parsed 2)])
          (seq [(nth parsed 3) (last parsed)]) (first parsed)])))

;;this is a piece of cloth
;;use (get-in @grid-atom [0 1]) to get square 0,1
(def grid-atom (atom [[[0,0] [0,1] [0,2]]
                      [[1,0] [1,1] [1,2]]
                      [[2,0] [2,1] [2,2]]]))

(def fabric (atom {}))

;;(def fabric (atom {0 { 0 1 1 2 2 3 }
;;                   1 { 0 2 1 3 2 5 }
;;                   2 { 0 5 1 3 2 2 }
;;                   3 { 0 3 1 2 2 4 }}))

;;((1 1) (2 2)) should return
;;{(1 1), (1 2), (2 1), (2 2)}

(defn entry->surface
  "takes a sanitized entry and returns all coordinates"
  [entry]
  (let [x (ffirst entry)
        y (second (first entry))
        p (first (second entry))
        q (second (second entry))]
    (for [x' (range x (if (zero? x) (+ x p) (+ x p)))
          y' (range y (if (zero? y) (+ y q) (+ y q)))]
      [x' y'])))

(defn entry->surface*
  "takes a sanitized entry and returns all coordinates"
  [entry]
  (let [claim-no (last entry)
        x (ffirst entry)
        y (second (first entry))
        p (first (second entry))
        q (second (second entry))]
    (for [x' (range x (if (zero? x) (+ x p) (+ x p)))
          y' (range y (if (zero? y) (+ y q) (+ y q)))]
      [x' y' claim-no])))

(defn surface-painter
  [[x y]]
  (let [sqinch (get-in @fabric [x y])]
    (if (nil? sqinch)
      (swap! fabric assoc-in [x y] 0)
      (swap! fabric assoc-in [x y] (inc sqinch)))))

(comment (map #(map surface-painter %) (->> input
                                            (map sanitize-entry)
                                            (map entry->surface)
                                            )))

;;107000 too low
;;too high Please wait one minute before trying again. (You guessed 158971.)
#_(count (distinct (reduce concat all-painted)))

(defn paint
  [line]
  (let [x      (ffirst line)
        y      (second (first line))
        db     @fabric
        sqinch (get-in db [x y])]
    (if (nil? sqinch)
      (swap! fabric assoc-in [x y] 0)
      (swap! fabric assoc-in [x y] (inc sqinch)))))


(defn solve-part-1
  [puz-in]
  (->> puz-in
       (map sanitize-entry)
       (map entry->surface) ;;return all claimed coordinates [[x y] [[x y] [x y]]]
       (reduce concat)      ;;flatten list [[x y][x y][x y]]
       frequencies          ;;count [[x y] 3]
       vals                 ;;             [3]
       (filter #(< 1 %))    ;;             [3]
       count))              ;;             [1]

(defn get-all-claimed
  "returns every claimed square inch"
  [puz-in]
  (->> puz-in
       (map sanitize-entry)
       (map entry->surface)
       (reduce concat)))

(defn find-duplicate
  [value coll]
  (some #(= value %) coll))

(defn solve-part-2
  "returns non-overlapping claims"
  [puz-in]
  (->> puz-in
       (map nice-entry)
       (map entry->surface*)
       (reduce concat)
       (group-by last)))

(comment
  (->> input
       (map nice-entry)
       (map entry->surface*)
       (reduce concat)
       (group-by first)
       frequencies
       (map group-by last)
       )

  (->> input
       (map nice-entry)
       (map entry->surface*)
       (reduce concat)
       (group-by first)
       frequencies
       (map #(second (first %))))

  (for [coordinate (->> poz
                        (map sanitize-entry)
                        (map entry->surface)
                        (reduce concat))]))
