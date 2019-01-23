(ns adventofcode-2017.day09
  (:require [clojure.string :as string :refer [split]]
            [adventofcode-2017.utils :as utils]))

(def puz-in(utils/read-all-lines-to-string "resources/2017/09.txt"))

(defn remove-ignored-chars "removes chars following bangs (!)"
  [s]
  (-> s
      (string/replace  #"!!" "")
      (string/replace  #"!." "")))


(defn remove-all-mess
  [s]
  (-> s
      (string/replace  #"!!" "")
      (string/replace  #"!." "")
      (string/replace  #"<.*?>" "")
      (utils/remove-commas)))

(defn garbage? [s] "helper method to check if something is garbage"
  (if (and (= (first s) \<)
           (= (last s) \>)) true false))

(defn find-garbage "finds and returns first contained garbage group" [s]
  (->> s
       (drop-while (complement #{\<}))
       string/join
       (re-find #"<.*?>")))

(defn clean-string
  ([s]
   (let [g (find-garbage s)]
     (if (garbage? g)
       (-> s
           (string/replace-first g "")
           (recur))
       s))))

;;keeps track of how many groups it's opened {{{ and uses that to count depth
;;messy with reverse's because don't know how to pop from strings
(defn tokenize "takes a clean string of {}'s, returns score"
  ([in] (tokenize in [] 0))
  ([in open points]
   (let [v     (vec (reverse in))
         fc    (first in)
         depth (count open)]
     (if (some? fc)
       (do
         (if (= \{ fc)
           (recur (reverse (pop v)) (conj open fc) points)
           (recur (reverse (pop v)) (pop open) (+ depth points))))
       points))))

(def clean-copy
  (remove-all-mess puz-in))

(def solution-A (tokenize clean-copy))

(defn remove-garbages "removes all garbage but leaves the <>"
  [^String s]
  (string/replace s #"<.*?>" "<>"))

;;subtract garbage from non-ignored-char input to get how many chars were deleted
(def solution-B
  (let [groups-and-garbage      (remove-ignored-chars puz-in)
        groups-empty-garbage (remove-garbages groups-and-garbage) ]
    (- (count groups-and-garbage)
       (count groups-empty-garbage))))
