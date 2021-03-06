(ns adventofcode-2018.day-11-test
  (:require [adventofcode-2018.day-11 :as sut]
            [clojure.test :refer [is are deftest]]))



(deftest find-power-level-test
  (are [input expected]
      (= (sut/find-power-level input) expected)
    [8 3 5]      4
    [57 122 79]  -5
    [39 217 196] 0
    [71 101 153] 4
    ))

(deftest sum-of-grid-test
  (are [input expected]
      (= (sut/sum-all-vals input) expected)
    [[4 4 4]
     [3 3 4]
     [1 2 4]] 29
    ))

(deftest solve-part-1
  ;;What is the X,Y coordinate of the top-left fuel cell
  ;;of the 3x3 square with the largest total power?
  (are [input expected]
      (=  (sut/solve-part-1 input) expected)
    18    29
    3999 31))
