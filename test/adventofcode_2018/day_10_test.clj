(ns adventofcode-2018.day-10-test
  (:require [adventofcode-2018.day-10 :as sut]
            [clojure.test :refer [is are deftest]]))


(deftest translate-test
  (are [input expected] (= expected
                           (sut/translate input))
    {:x 9  :y 1 :dx 5 :dy  1}  {:x 14 :y 2 :dx 5 :dy 1}
    {:x 3  :y 9 :dx 1 :dy -2}  {:x 4 :y 7 :dx 1 :dy -2}
    ))
