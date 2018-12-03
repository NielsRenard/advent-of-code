(ns adventofcode-2018.day-03-test
  (:require [adventofcode-2018.day-03 :as sut]
            [clojure.test :refer [deftest is]]))


(deftest solve-part-1
  ;;The four square inches marked with X are claimed
  ;;by both 1 and 2. (Claim 3, while adjacent to the
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

;; ........
;; ...2222.
;; ...2222.
;; .11XX22.
;; .11XYX2.
;; .111X3..
;; .1111...
;; ........
(deftest solve-part-1-three-things-overlap
  (is (= 6
         (sut/solve-part-1 ["#1 @ 1,3: 4x4"
                            "#2 @ 3,1: 4x4"
                            "#3 @ 4,4: 2x2"]))))

;; Y.......
;; 5.......
;; X.......
;; ........
;; ........
;; ........
;; ........
;; ........
(deftest solve-part-1-four-things-overlap
  (is (= 2
         (sut/solve-part-1 ["#1 @ 0,0 1x1"
                            "#2 @ 0,0 1x1"
                            "#3 @ 0,0 1x1"
                            "#4 @ 0,0 1x1"
                            "#5 @ 0,0 1x3"
                            "#6 @ 0,2 1x1"]))))



;; 1111....
;; 111X222.
;; 111X222.
;; 111X222.
;; ...2222.
;; .....33.
;; .....33.
;; ........
(deftest solve-part-1-with-zeroes
  (is (= 3
         (sut/solve-part-1 ["#1 @ 0,0: 4x4"
                            "#2 @ 3,1: 4x4"
                            "#3 @ 5,5: 2x2"]))))

;; 11111...
;; 111XX22.
;; 111XX22.
;; 111XX22.
;; ...2222.
;; .....33.
;; .....33.
;; ........
(deftest solve-part-1-width-bigger-than-height
  (is (= 6
         (sut/solve-part-1 ["#1 @ 0,0: 5x4"
                            "#2 @ 3,1: 4x4"
                            "#3 @ 5,5: 2x2"]))))

;; 1111....
;; 111X222.
;; 111X222.
;; 111X222.
;; 111X222.
;; .....33.
;; .....XX4
;; .....444
(deftest solve-part-1-height-bigger-than-width
  (is (= 6
         (sut/solve-part-1 ["#1 @ 0,0: 4x5"
                            "#2 @ 3,1: 4x4"
                            "#3 @ 5,5: 2x2"
                            "#4 @ 5,6: 20x20"]))))
