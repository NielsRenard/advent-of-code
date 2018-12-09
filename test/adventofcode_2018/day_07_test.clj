(ns adventofcode-2018.day-07-test
  (:require [adventofcode-2018.day-07 :as sut]
            [clojure.test :refer [is are deftest]]))


;; I added the last line because the input did not cover
;; the case of multiple starting nodes.
(def example-input
  ["Step C must be finished before step A can begin."
   "Step C must be finished before step F can begin."
   "Step A must be finished before step B can begin."
   "Step A must be finished before step D can begin."
   "Step B must be finished before step E can begin."
   "Step D must be finished before step E can begin."
   "Step F must be finished before step E can begin."
   "Step Q must be finished before step A can begin."])

(deftest solve-part-1
  (is (= [:C :Q :A :B :D :F]
         (sut/solve-part-1 example-input))))
