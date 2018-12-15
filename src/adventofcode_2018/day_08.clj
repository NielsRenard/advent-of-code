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
(comment (def example-node
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
                :meta-data [2 1 1 2]}}))

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
       (tree-seq :children :children)
       (map :meta)
       flatten
       (apply +)
       ))

(def db (atom '()))

(defn node-value
  [aggr root-node]
  (let [subnode-index (:meta root-node)]
    (if (some? subnode-index)
      (let [sub-nodes (filter coll? (for [sni subnode-index]
                                      (-> root-node
                                          :children
                                          (nth ,,, (dec sni) nil))))]
        (if (empty? sub-nodes)
          aggr
          (for [sn sub-nodes]
            (if (contains? sn :children)
              (take (count sub-nodes) (iterate #(node-value aggr %) sn))
              (cons aggr (:meta sn))
              )))))))

(defn solve-part-2
  [puz-in]
  (let [tree      (parse-tree puz-in)
        root-node (first (tree-seq (juxt seq :children) :children tree))]
    (->> (node-value 0 root-node)
         flatten
         (remove coll?)
         (remove nil?)
         (remove zero?)
         )))

(comment
  ;; part 2:
  ;; too low 36505

  ;;part 1:
  ;; too low 7146
  ;; wrong 7155
  ;; was not going depth first

  ;; this went nowhere
  (+ (get-tail-end-meta input) (reduce + (flatten (trampoline parse-tree [] input))))
  7146

  (parse-tree (cons children (into {:meta (into [] (take num-meta more))}))
              (nthrest more num-meta)))
