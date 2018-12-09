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


;;          this is what it becomes
;;
;;         {:A #{:O :Q :E :Y},
;;          :B #{:F :Q :Y :V},
;;          :C #{:A :W :H},
;;          :D #{:O :Q :E :H},
;;          :E #{:Z :Y},
;;          :F #{:O :Z :E},
;;          :G #{:I :O :D :B :X :H},
;;          :H #{:R :Z :E :Y},
;;          :I #{:O :A :W :J :Z :V :N},
;;          :J #{:M :R :O :A :C :H},
;;          :K #{:A :H :V},
;;          :L #{:V :U},
;;          :M #{:R :W :Q :Z :E},
;;          :N #{:F :Q :D :C :H :V},
;;          :O #{:R :W :Q :Y},
;;          :P #{:O :J :Z :C :U},
;;          :Q #{:R :Z},
;;          :R #{:Z},
;;          :S #{:L :R :A :E},
;;          :T #{:I :F :B :Y :X :U :S :N},
;;          :U #{:F :W :C},
;;          :V #{:Q :Z :Y :H},
;;          :W #{:Q :Z :Y},
;;          :X #{:I :B :J :E :Y},
;;          :Y #{:R :Q :Z}}


(defn find-all-used-letters
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
        all-steps-used (find-all-used-letters instructions)]
    (->> instructions
         (map (comp seq val))
         flatten
         set
         (diff all-steps-used)
         first
         sort
         ;; TODO: can call first here again and rename to find-next-step
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
  (let [;; TODO: possible-steps can be refactored away with next-step
        possible-steps   (->> parsed-data
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
  "It looks through one-round until there's no more input"
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
