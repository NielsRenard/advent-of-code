(ns adventofcode-2017.day04
  (:require [clojure.string :as string]
            [adventofcode-2017.utils :as utils]))

(def valid-phrase "aa aaa bb bbbb")
(def invalid-phrase "inb zum zmu dnl zjxg vrdziq ypdnsvt")

(def puz-in (utils/read-all-lines "resources/2017/04.txt"))

(defn split-phrase
  [phrase]
  (string/split phrase #" "))

(defn- valid-phrase?
  [y]
  (=(count (set (split-phrase y))) (count (split-phrase y))))

(defn- word-contains-char?
  [word char]
  (filter true? (map #(string/includes? % char) word)))

(defn- string-contains-char
  [word char]
  (.contains word (str char)))

(defn- anagram?
  [w1 w2]
  (every? true? (map #(string-contains-char w2 %) w1)))

(anagram? "lalo" "allo")

(defn- no-anagrams-in-phrase?
  "true if none of the words are anagrams of each other"
  [p]
  (map anagram? (split-phrase p)))

(no-anagrams-in-phrase? invalid-phrase)

(defn solution
  "Solution to 4a"
  [input]
  (count(filter true?(map (comp valid-phrase?) input))))

(def solution-A (solution puz-in))
