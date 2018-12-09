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
    ;;refactor this :must keyword out of the vector
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
         (map (comp seq val)) ;; get the unlockable steps [:A [:C :B] [:D :C]]
         flatten                 ;;               [:A  :C :B   :D :C ]
         (concat first-row)      ;; combine steps with no pre-req
         set                     ;; dedupe       #{:A  :B :C :D}
         ;;delet this
         #_(diff alphabet)         ;; diff against alphabet
         #_last                    ;; take the common keys
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

;; TODO: too much nesting, refactor '(:must (:C) :before [:A :F]
;; to just {:C [:A :F]}
(defn get-unlocks
  "takes a step and returns what other steps they unlock."
  [step parsed-data]
  (->> parsed-data
       step))

;; TODO: think of better names
(defn remove-satisfied-befores
  "Removes keys from :before column in the parsed data."
  [keys parsed-data]
  (->> parsed-data
       (map #(update % :before (fn [coll] (remove keys coll))))))


(defn remove-satisfied-steps
  "If the before collection is empty, remove the whole entry."
  [parsed-data]
  (let [answer (->> parsed-data
                    (filter (comp not-empty :before)))]
    answer))

(def example-input
  ["Step C must be finished before step A can begin."
   "Step C must be finished before step F can begin."
   "Step A must be finished before step B can begin."
   "Step A must be finished before step D can begin."
   "Step B must be finished before step E can begin."
   "Step D must be finished before step E can begin."
   "Step F must be finished before step E can begin."])


#_(defn solve-part-1
    [puz-in]
    (let [parsed-data (parse-data example-input)]
      (->>  parsed-data
            find-possible-steps
            first
            (map #(get-unlocks % parsed-data))
            )))

(def bed (atom #{}))

(defn can-be-removed?
  [key parsed-data]
  (->> parsed-data
       :before
       #_       flatten
       #_(some #{key})))

(defn remove-finished-steps
  [finished-steps parsed-data]
  (->> (first finished-steps)
       #_(apply dissoc parsed-data)
       (dissoc parsed-data)))

(defn one-round
  [parsed-data]
  (let [possible-steps   (->> parsed-data
                              find-possible-steps)
        possible-unlocks (->>  possible-steps
                               (map #(get-unlocks % parsed-data))
                               (map (comp flatten seq))
                               set
                               )]
    (if (= 1 (count parsed-data))
      ;; figure out a nicer way to return the secondlast and last
      [(str (first possible-steps) (ffirst possible-unlocks)) (->> parsed-data
                                                                   (remove-finished-steps possible-steps))]
      [(first possible-steps)    (->> parsed-data
                                      (remove-finished-steps possible-steps))])))

(defn solve-part-1
  ([input]
   (solve-part-1 nil [:starting input]))    ;; 1 arg self calling fn to kick it off
  ([answer remaining-data]
   (let [step      (first remaining-data)
         remaining (second remaining-data)]
     (if (nil? step)                        ;; break out of the recur when no next steps
       (->> answer
            reverse
            rest
            (map name)
            (apply str)
            (#(clojure.string/replace % ":" ""))) ;; hack out the keyword : back to string

       (->> remaining
            one-round
            (recur (conj answer step))      ;; add the step to the answer and run again
            )))))

(comment
  ;;wrong, didn't take into account mid-step shifts
  ;;GKPTSXBILJNUCDFMVAHEOWYQR
  ;;GKPTSLUXBIJMNCADFOVHEWYQR
  (parse-data-structure input)
  (parse-data-structure example-input)
  (map (fn [ul pd] (can-be-removed? ul pd)) new-unlocks)
  (swap! bed union (first possible-steps) )
  ;; this does nothing because possible-steps are already out of :before
  (remove-satisfied-befores (first possible-steps)))
