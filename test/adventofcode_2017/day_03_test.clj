(ns adventofcode-2017.day03-test
  (:require  [adventofcode-2017.day03 :as sut]
             [clojure.test :refer [deftest testing is are] :as t]))

(deftest part-1
  (testing "nothing"
    (are [input expected] (= (sut/nothing) expected)
      [3 2 1] 2
      )))
