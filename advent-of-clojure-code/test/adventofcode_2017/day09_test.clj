(ns adventofcode-2017.day09-test
  (:require [adventofcode-2017.day09 :as sut]
            [clojure.test :as t :refer [deftest is testing are]]))

(deftest garbage?-test
  (testing "returns true if string is garbage"
    (are [input expected] (= expected (sut/garbage? input))
      nil  false
      "<>" true
      #_#_ "<1d2>"     true
      #_#_ "<!>>"      true
      #_#_ "<f,sefj}>" true)
    ))

(deftest remove-ignored-chars-test
  (testing "it works"
    (are [input expected] (= expected (sut/remove-ignored-chars input))
      "{{!!!<}}"                    "{{}}"
      "{!!!a}"                    "{}"
      "{{<!>},{<!>},{<!>},{<a>}}" "{{<},{<},{<},{<a>}}"
      )))

(deftest find-garbage-test
  (testing "it works"
    (are [input expected] (= expected (sut/find-garbage input))
      "{<>}"   "<>"
      "{<ee>}" "<ee>"
      "{{{<@#$}" nil
      )))

(deftest clean-string-test
  (testing "it works"
    (are [input expected] (= expected (sut/clean-string input))
      "{{<ala{<}lal>}}" "{{}}")))

#_( group?-test
  (testing "returns true if string is group"
    (are [input expected] (= expected (sut/group? input))
      nil     false
      "group" false
      #_#_"{}"    true
      #_$_"{!}"   false)))

(deftest tokenize-test
  (testing "counts points correctly"
    (are [input expected] (= expected (sut/tokenize input))
      "{}"           1
      "{{{}}}"       6
      "{{}{}}"       5
      "{{{}{}{{}}}}" 16
      "{{}{}{{}}}"   10
      ;;todo add imbalanced test
      )))
