(ns adventofcode-2018.day-09-test
  (:require [adventofcode-2018.day-09 :as sut]
            [clojure.test :refer [is are deftest]]))

(deftest solve-part-1-test
  (is (= 138
         (-> (sut/solve-part-1 )))))
