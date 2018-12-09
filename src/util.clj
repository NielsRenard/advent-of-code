(ns util
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

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
