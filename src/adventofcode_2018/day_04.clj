(ns adventofcode-2018.day-04
  (:require [util :refer :all]
            [clojure.string :as string]
            [java-time :as t]))

(def input (get-input))


(defn parse-date [line]
  (t/local-date "yyyy-MM-dd" (-> line
                                 (string/replace "[" "")
                                 (string/replace "]" "")
                                 (string/split #" ")
                                 first)))

(defn solve-part-1
  [puz-in]
  -1)
