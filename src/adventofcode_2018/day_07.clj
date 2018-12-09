(ns adventofcode-2018.day-07
  (:require [clojure.string :as string]
            [util :refer :all]
            [clojure.data :refer :all]
            [clojure.set :refer :all]))

(def input (util/get-input))

(defn parse-step [line]
  "parses this string:
      Step A must be finished before step B can begin.
   into an 'instruction' map:
      {:step [:A] :pre-req [:B]}"
  (let [split (-> line
                  (string/replace "Step " "")
                  (string/replace " must be finished before step" "")
                  (string/replace " can begin." "")
                  (string/split #" ")
                  (#(map keyword %))
                  )]
    {(first split) #{(second split)}}))

(defn parse-data [puz-in]
  "Parses and cleans up the input to a seq of instructions"
  (->> puz-in
       (map parse-step)
       (apply merge-with into (sorted-map))))

(defn all-used-keywords
  "returns a coll of all used keywords"
  [instructions]
  (let [alphabet  (set (map (comp keyword str) (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
        first-row (->> instructions
                       (map first)
                       flatten
                       distinct)]
    (->> instructions
         (map (comp seq val))    ;; get the unlockable steps [:A [:C :B] [:D :C]]
         flatten                 ;;               [:A  :C :B   :D :C ]
         (concat first-row)      ;; combine steps with no pre-req
         set                     ;; dedupe       #{:A  :B :C :D}
         )))


(defn find-possible-steps
  "Finds the next possible steps: the one that are not in the list of 'unlockable' steps."
  [instructions]
  (let [alphabet       (set (map (comp keyword str) (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
        all-steps-used (all-used-keywords instructions)]
    (->> instructions
         (map (comp seq val))
         flatten
         set
         (diff all-steps-used)
         first
         sort
         )))

(defn get-unlocks
  "takes a step and returns what other steps they unlock."
  [step parsed-data]
  (->> parsed-data
       step))

(defn remove-finished-step
  [finished-step parsed-data]
  (dissoc parsed-data finished-step))

(defn one-round
  [parsed-data]
  (let [possible-steps   (->> parsed-data
                              find-possible-steps)
        next-step (->> parsed-data
                       find-possible-steps
                       first
                       )
        possible-unlocks (->>  possible-steps
                               (map #(get-unlocks % parsed-data))
                               (map (comp flatten seq))
                               set
                               )]
    (if (= 1 (count parsed-data))
      ;; figure out a nicer way to return the secondlast and last
      [(str next-step (ffirst possible-unlocks))
       (remove-finished-step next-step parsed-data)]

      [next-step    (remove-finished-step next-step parsed-data)])))

(defn solve-part-1
  ([input]
   (solve-part-1 nil [:starting input]))          ;; 1 arg self calling fn to kick it off
  ([answer remaining-data]
   (let [step      (first remaining-data)
         remaining (second remaining-data)]
     (if (nil? step)                              ;; break out of the recur when no next steps
       (->> answer
            reverse
            rest
            (map name)
            (apply str)
            (#(clojure.string/replace % ":" ""))) ;; hack out the keyword : back to string

       (->> remaining
            one-round
            (recur (conj answer step))            ;; add the step to the answer and run again
            )))))
