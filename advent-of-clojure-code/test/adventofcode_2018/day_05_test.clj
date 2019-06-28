(ns adventofcode-2018.day-05-test
  (:require [adventofcode-2018.day-05 :as sut]
            [clojure.test :refer [deftest is are]]))


(deftest react?-test
  (are [charpair expected] (= expected (sut/react? charpair))
    [\a \A] true
    [\A \a] true
    [\a \a] false
    [\A \A] false
    [\z \Z] true
    [\Z \z] true))

#_(deftest solve-part-1-test
    (are [input expected] (= expected (sut/solve-part-1 input))
      ;;    ;;In aA, a and A react, leaving nothing behind.
      ;;    "aA"     ""
      ;;    ;;In abBA, bB destroys itself, leaving aA. As above,
      ;;    ;;this then destroys itself, leaving nothing.
      ;;    "abBA"   ""
      ;;    ;;In abAB, no two adjacent units are of the same type,
      ;;    ;;and so nothing happens.
      ;;    "abAB"   "abAB"
      ;;    ;;In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.
      ;;    "aabAAB" "aabAAB"
      ;;
      ;;    ;;Now, consider a larger example, dabAcCaCBAcCcaDA:
      ;;    ;;dabAcCaCBAcCcaDA  The first 'cC' is removed.
      ;;    ;;dabAaCBAcCcaDA    This creates 'Aa', which is removed.
      ;;    ;;dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
      ;;    ;;dabCBAcaDA        No further actions can be taken.
      ;;    "dabAcCaCBAcCcaDA" "dabCBAcaDA"
      "AabAcaCBAaccDAa" "bAcaCBccD"))
