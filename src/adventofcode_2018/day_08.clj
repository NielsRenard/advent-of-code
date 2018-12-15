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

(defn sum-all-meta [coll]
  (reduce + coll))

(defn p-child
  [children [num-children num-meta & more]]
  (let []
    (if (zero? num-children)
      #(p-node (into children (take num-meta more))
               (nthrest more num-meta))
      #(p-node children more))))

(defn p-node [children node]
  (let [num-children (first node)
        num-meta     (second node)
        more         (nthrest node 2)]
    (if (nil? node)
      children
      (if (zero? num-children)
        #(p-child children more)
        #(p-child children more)))))


(defn solve-part-1
  [puz-in]
  -1
  )

(comment
  ;; too low 7146
  ;;wrong 7155
  (+ (get-tail-end-meta input) (reduce + (flatten (trampoline p-node [] input))))
  7146

  (p-node (cons children (into {:meta (into [] (take num-meta more))}))
          (nthrest more num-meta)))
