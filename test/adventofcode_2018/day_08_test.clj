(ns adventofcode-2018.day-08-test
  (:require [adventofcode-2018.day-08 :as sut]
            [clojure.test :refer [is are deftest]]))

(def example-input
  '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

                                        ;2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
                                        ;A----------------------------------
                                        ;B----------- C-----------
                                        ;D-----

                                        ;In this example, each node of the tree is also marked with an underline
                                        ;starting with a letter for easier identification. In it, there are four nodes:
                                        ;
                                        ;A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
                                        ;B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
                                        ;C, which has 1 child node (D) and 1 metadata entry (2).
                                        ;D, which has 0 child nodes and 1 metadata entry (99).
                                        ;
                                        ;The first check done on the license file is to simply add up all of the
                                        ;metadata entries. In this example,
                                        ;that sum is 1+1+2+10+11+12+2+99=138.

(def better-example-input
  '(3 4 0 3 1 2 5 2 2 1 1 0 2 3 3 1 0 3 1 2 4 1 2 1 2 0 1 2 1 2 1 1 2 3))

(deftest solve-part-1-test
  (is (= 138
         (-> (sut/solve-part-1 example-input)))))

(deftest solve-part-2-test
  (is (= 66
         (-> (sut/solve-part-2 example-input)))))

(deftest solve-part-2-test-b
  (is (= 31
         (-> (sut/solve-part-2 better-example-input)))))
