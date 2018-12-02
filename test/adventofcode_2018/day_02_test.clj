(ns adventofcode-2018.day-02-test
  (:require [adventofcode-2018.day-02 :as sut]
            [clojure.test :refer [deftest is]]))

(deftest solve-part-1
  ;;"Of these box IDs, four of them contain a letter which
  ;;appears exactly twice, and three of them contain a letter
  ;;which appears exactly three times. Multiplying these together
  ;;produces a checksum of 4 * 3 = 12."
  (is (= 12
         (sut/solve-part-1 ["abcdef"
                            "bababc"
                            "abbcde"
                            "abcccd"
                            "aabcdd"
                            "abcdee"
                            "ababab"]))))

(deftest solve-part-2
  ;; The IDs abcde and axcye are close, but they differ by two characters
  ;; (the second and fourth). However, the IDs fghij and fguij differ by
  ;; exactly one  character, the third (h and u). Those must be the correct boxes.
  (is (= "fgij"
         (sut/solve-part-2 ["abcde"
                            "fghij"
                            "klmno"
                            "pqrst"
                            "fguij"
                            "axcye"
                            "wvxyz"]))))
