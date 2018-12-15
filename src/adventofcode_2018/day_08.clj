(ns adventofcode-2018.day-08
  (:require [util :refer :all]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def input (->> (first (util/get-input))
                split-by-whitespace
                (map edn/read-string)))


(def example-input
  '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(def parsed-ex
  [[[2 3]   [[[0 3] [] [10 11 12]]
             [[1 1] [[0 1] [] [99]] [2]]]   [1 1 2]]])

;; a node tree could look like this;
(def example-node
  {:A {:header    [2 3]
       :children  {:B {:header    [1 1]
                       :children  []
                       :meta-data [10 11 12]}
                   :C {:header    [3 10]
                       :children
                       {:D {:header    [0 1]
                            :children  []
                            :meta-data [99]}}
                       :meta-data [2]}}
       :meta-data [2 1 1 2]}})

(defn get-tail-end-meta [inp]
  (nthrest inp (+ 2 (.lastIndexOf inp 0))))

(def tail-meta
  (get-tail-end-meta input))

(defn parse-tree [[num-children num-meta & remaining]]
  (let []
    (if (zero? num-children)
      {:meta      (take num-meta remaining)
       :remaining (drop num-meta remaining)}
      (let [children  (->> {:remaining remaining}
                           (iterate #(parse-tree (:remaining %)))
                           (drop 1)
                           (take num-children))
            root-meta (->> children
                           last
                           :remaining
                           (take num-meta))]
        {:meta      root-meta
         :children  children
         :remaining (drop num-meta (:remaining (last children)) )}))))

(defn solve-part-1
  [puz-in]
  (->> (parse-tree puz-in)
       (tree-seq (juxt seq :children) :children)
       (map :meta)
       flatten
       (apply +)
       ))

(comment
  ;; too low 7146
  ;; wrong 7155
  ;; was not going depth first

  ;; this went nowhere
  (+ (get-tail-end-meta input) (reduce + (flatten (trampoline parse-tree [] input))))
  7146

  (parse-tree (cons children (into {:meta (into [] (take num-meta more))}))
              (nthrest more num-meta)))
