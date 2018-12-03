(ns adventofcode-2018.day-01-test
  (:require [adventofcode-2018.day-01 :as sut]
            [clojure.test :refer [deftest is are]]))

(deftest solve-part-1
  ;;Starting with a frequency of zero, what is the resulting
  ;;frequency after all of the changes in frequency have been applied?
  (are [input expected]
      (= (sut/solve-part-1 input) expected)

    [+1 -2 +3 +1] 3
    [+1 +1 +1]    3
    [+1 +1 -2]    0
    [-1 -2 -3]   -6))

(deftest solve-part-2
  ;; You notice that the device repeats the same frequency change
  ;; list over and over. To calibrate the device, you need to find
  ;; the first frequency it reaches twice.
  (are [input expected]
      (= (sut/solve-part-2 input) expected)

    [+1 -2 +3 +1 +1 +1 -2] 2
    [+1 -1]                0
    [-6 +3 +8 +5 -6]       5
    [+7 +7 -2 -7 -4]      -6))
