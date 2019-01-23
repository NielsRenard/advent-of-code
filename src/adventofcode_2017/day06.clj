(ns adventofcode-2017.day06
  (:require [adventofcode-2017.utils :as utils]
            [clojure.string :as string]))

(def puz-in (utils/read-all-lines-to-string "resources/2017/06.txt"))

(def input (string/split puz-in #"\t"))
