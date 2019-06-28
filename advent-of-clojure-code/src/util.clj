(ns util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn get-input
  "Returns a line-seq of the puzzle input for the year and day in *namespace*.
  Optionally provide a path to override filename deduction."
  ([] (let [year     (re-find #"\d{4}" (str *ns*))
            day      (re-find #"\d{2}$" (str *ns*))
            filename (str year "/input_" day ".txt")]
        (try (doall (-> filename
                        io/resource
                        io/reader
                        line-seq))
             (catch Exception ex
               (str "Exception occurred trying to read puzzle input. Does the file exist?")))))

  ([filepath] (-> filepath
                  io/resource
                  io/reader
                  line-seq)))

(defn split-by-whitespace [s]
  (clojure.string/split s #"\s+"))

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
  (string/split s #"\t"))

(defn string->int [s]
  (Integer/parseInt s))

(defn string->ints [s]
  "takes a string of numbers and return a vec of integers"
  (mapv string->int s))


;;performance seems the same for these two
(defn remove-non-alphabet "removes anything but alphabet chars" [s] (apply str (re-seq #"[a-zA-Z]" s)))

(defn remove-commas [s] (string/replace s "," ""))
