(ns adventofcode-2017.utils
  (:require [clojure.string :as s]))

(defn read-all-lines
  "Requires a filepath, returns a seq of all lines in said file."
  [filepath]
  (with-open [rdr (clojure.java.io/reader filepath)]
    (doall(line-seq rdr))))

(defn read-all-lines-to-int
  [filepath]
  (map read-string(with-open [rdr (clojure.java.io/reader filepath)]
                    (doall(line-seq rdr)))))

(defn read-all-lines-to-string
  [filepath]
  (apply str (read-all-lines filepath)))

(defn split-on-tabs
  [s]
  (s/split s #"\t"))

(defn string->int [s]
  (Integer/parseInt s))

(defn string->ints [s]
  "takes a string of numbers and return a vec of integers"
  (mapv string->int s))


;;performance seems the same for these two
(defn remove-non-alphabet "removes anything but alphabet chars" [s] (apply str (re-seq #"[a-zA-Z]" s)))

(defn remove-commas [s] (s/replace s "," ""))
