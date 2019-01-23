(ns adventofcode-2017.day08
  (:require [adventofcode-2017.utils :as utils]
            [clojure.string :as string]))

(def puz-in (into [] (utils/read-all-lines "resources/2017/08.txt")))


(def example-input (take 5 puz-in))

(def ex1 "a inc 5 if b == 0")
(def ex2 "sd dec 441 if k != 0")

(def register {:a 0 :b 0 :c 0})

(defn- get-register-value
  "gets the value of the given register key"
  [key]
  ;;(get register (read-string (str ":" (read-string key))))
  (get register key))

(defn read-instruction
  "Reads and executes a registry instruction."
  [instruction]
  (let [a (get-register-value (keyword(read-string instruction)))
        operation (->> (string/split instruction #"\s")
                       (take 3)
                       (drop 1))
        predicate (map read-string (take-last 3 (string/split instruction #"\s") ))
        b (get-register-value (keyword(first predicate)))]

    (if (contains? register (keyword (first predicate)))
      (println  "predicate key  in map")
      (def register (assoc register (keyword (first predicate)) 0)))
    (if (contains? register (keyword(read-string instruction)))
      (println  "key in map")
      (def register (assoc register (keyword(read-string instruction)) 0)))
    (if (true? (eval(list (if (= (read-string "!=") (second predicate))
                            (read-string "not=")
                            (second predicate))
                          b (last predicate))))

      ;;figure out how to eval the operation and apply it to the subject

      (def register (assoc register (keyword(read-string instruction)) (if (=  (first operation) "inc")
                                                                         (+ a (read-string (second operation)))
                                                                         (- a (read-string (second operation)))))))))

(comment  (read-instruction ex1)
          (def first-three ["sd dec 441 if k != 0" "lp dec 419 if mxn >= 7" "w inc -592 if icg >= -9"])
          (map read-instruction first-three)
          register
          (apply max-key val register))
