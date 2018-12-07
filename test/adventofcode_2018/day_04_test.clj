(ns adventofcode-2018.day-04-test
  (:require [adventofcode-2018.day-04 :as sut]
            [clojure.test :refer [deftest is are]]))

;; Note that guards count as asleep on the minute they fall asleep, and they count
;; as awake on the minute they wake up. For example, because Guard #10 wakes up at
;; 00:25 on 1518-11-01, minute 25 is marked as awake.

;; Visually, these records show that the guards are asleep at these times:
;;
;; Date   ID   Minute
;;             000000000011111111112222222222333333333344444444445555555555
;;             012345678901234567890123456789012345678901234567890123456789
;; 11-01  #10  .....####################.....#########################.....
;; 11-02  #99  ........................................##########..........
;; 11-03  #10  ........................#####...............................
;; 11-04  #99  ....................................##########..............
;; 11-05  #99  .............................................##########.....
;; Strategy 1: Find the guard that has the most minutes asleep. What minute
;; does that guard spend asleep the most?  In the example above, Guard #10 spent
;; the most minutes asleep, a total of 50 minutes (20+25+5), while Guard #99 only
;; slept for a total of 30 minutes (10+10+10). Guard #10 was asleep most during
;; minute 24 (on two days, whereas any other minute the guard was asleep was only
;; seen on one day).

(def example-input
  ["[1518-11-01 00:00] Guard #10 begins shift"
   "[1518-11-01 00:05] falls asleep"
   "[1518-11-01 00:25] wakes up"
   "[1518-11-01 00:30] falls asleep"
   "[1518-11-01 00:55] wakes up"
   "[1518-11-01 23:58] Guard #99 begins shift"
   "[1518-11-02 00:40] falls asleep"
   "[1518-11-02 00:50] wakes up"
   "[1518-11-03 00:05] Guard #10 begins shift"
   "[1518-11-03 00:24] falls asleep"
   "[1518-11-03 00:29] wakes up"
   "[1518-11-04 00:02] Guard #99 begins shift"
   "[1518-11-04 00:36] falls asleep"
   "[1518-11-04 00:46] wakes up"
   "[1518-11-05 00:03] Guard #99 begins shift"
   "[1518-11-05 00:45] falls asleep"
   "[1518-11-05 00:55] wakes up"])

(deftest solve-part-1
  (is (= (* 10 24)
         (sut/solve-part-1 example-input))))

(deftest solve-part-2
  (is (= (* 99 45)
         (sut/solve-part-2 example-input))))

;;testing helper functions
(deftest get-guard-ids-test
  (is (=  '(10 99)
          (sut/get-all-guard-ids example-input))))

(deftest find-most-slept-minute-for-guard-test
  (are [guard expected] (= expected
                           (sut/find-most-slept-minute-of-guard example-input guard))
    10 [24 2]
    99 [45 3]))
