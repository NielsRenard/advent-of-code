(ns adventofcode-2017.day01-test
  (:require [adventofcode-2017.day01 :as sut]
            [clojure.test :refer [is are] :as t]))

(is (= (sut/solve) 1029))


(is (= (sut/solution [1 1 1 1])
       4))

(are [input expected] (= (sut/solution input) expected)
  [1 2 2 3] 2
  [1 1 1 1] 4
  [1 2 2 3] 2)

(are [input expected] (= (sut/halfway-equal? 0 0 input) expected)
  [1 2 1 2] 6)
