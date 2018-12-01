(ns adventofcode-2018.day-01-test
  (:require [adventofcode-2018.day-01 :as sut]
            [clojure.test :refer [deftest is are]]))

(deftest solve-part-1
  (are [input expected]
      (= (sut/solve-part-1 input) expected)

    [+1 -2 +3 +1] 3
    [+1 +1 +1]    3
    [+1 +1 -2]    0
    [-1 -2 -3]   -6))

(deftest solve-part-2
  (are [input expected]
      (= (sut/solve-part-2 input) expected)

    [+1 -2 +3 +1 +1 +1 -2] 2
    [+1 -1]                0
    [-6 +3 +8 +5 -6]       5
    [+7 +7 -2 -7 -4]      -6))
