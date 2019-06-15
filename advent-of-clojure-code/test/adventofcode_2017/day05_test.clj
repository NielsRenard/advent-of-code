(ns adventofcode-2017.05test
  (:require  [clojure.test :as t])
  (:require  [ adventofcode-2017.day05 :as sut]))

(def example-input [0
                    3
                    0
                    1
                    -3])

(t/is (= (sut/better-step-counter 0 0 example-input) 5))

(t/is (= (sut/part-two-step-counter 0 0 example-input) 10))
