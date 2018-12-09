(ns adventofcode-2018.day-08-test
  (:require [adventofcode-2018.day-08 :as sut]
            [clojure.test :refer [is are deftest]]))

(def example-input
  '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(deftest solve-part-1-test
  (is (= 138
         (-> (sut/solve-part-1 example-input )))))
