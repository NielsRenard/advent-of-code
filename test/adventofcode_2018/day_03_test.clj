(ns adventofcode-2018.day-03-test
  (:require [adventofcode-2018.day-03 :as sut]
            [clojure.test :refer [deftest is]]))


(deftest solve-part-1
  ;;The four square inches marked with X are claimed
  ;;by both 1 and 2. (Clim 3, while adjacent to the
  ;;others, does not overlap either of them.)
  ;; Visually, these claim the following areas:
  ;; ........
  ;; ...2222.
  ;; ...2222.
  ;; .11XX22.
  ;; .11XX22.
  ;; .111133.
  ;; .111133.
  ;; ........
  (is (= 4
         (sut/solve-part-1 ["#1 @ 1,3: 4x4"
                            "#2 @ 3,1: 4x4"
                            "#3 @ 5,5: 2x2"]))))
