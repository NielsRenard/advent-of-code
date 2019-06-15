(ns adventofcode-2017.day02-test
  (:require  [adventofcode-2017.day02 :as sut]
             [clojure.test :refer [deftest testing is are] :as t]))

(deftest part-1
  (testing "diff-min-and-max"
    (are [input expected] (= (sut/diff-min-and-max input) expected)
      [1 2]   1
      [2 1]   1
      [1 2 3] 2
      [3 2 1] 2
      ))
  (testing "sum-of-diffs"
    (are [input expected] (= (sut/sum-of-diffs input) expected)
      [[1]
       [2]
       [3]] 0

      [[5 1 9 5]
       [7 5 3  ]
       [2 4 6 8]] 18)))

(deftest part-2
  (testing "divisable?"
    (are [a b expected] (= (sut/divisable? a b) expected)
      3 1 true
      8 2 true
      4 9 false
      ))
  (testing "find-divisable"
    (are [input expected] (= (sut/find-divisable input) expected)
      [5 9 2 8] (seq [2 8])))
  (testing "solution"
    (are [input expected] (= (sut/solve input) expected)
      [[5 9 2 8]
       [9 4 7 3]
       [3 8 6 5]] 9)))
