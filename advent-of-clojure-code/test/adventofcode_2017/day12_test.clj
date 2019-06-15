(ns adventofcode-2017.day12-test
  (:require [adventofcode-2017.day12 :as sut]
            [clojure.test :as t :refer [deftest is testing are]]))

(deftest vec->mapofsets
  (testing "probably better to convert all to numbers "
    (are [input expected] (= expected (sut/vec->mapofsets input ))
      ["31" "<->" "21"] {"31" #{"21"}}
      )))
