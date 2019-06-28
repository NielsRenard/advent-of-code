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

;;  result of running
;;  (parse-tree example-input)
;;
;;  {:meta (1 1 2),
;;   :children
;;   ({:meta (10 11 12), :remaining (1 1 0 1 99 2 1 1 2)}
;;    {:meta (2),
;;     :children ({:meta (99), :remaining (2 1 1 2)}),
;;     :remaining (1 1 2)}),
;;   :remaining ()}

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

(defn node-value
  ([root-node] (node-value 0 root-node))                  ;; one arg starter
  ([aggr root-node]                                       ;;
   (let [sub-nodes (for [idx (:meta root-node)]           ;; use the meta-data as indexes
                     (-> (:children root-node)            ;; check all child-nodes for existing index
                         (nth ,,, (dec idx) nil)))]       ;;
     (if (empty? sub-nodes)                               ;; if nothing comes out, we're done
       aggr                                               ;; and return from here
       (->> sub-nodes                                     ;;
            (map (fn [sn]                                 ;; map over each node
                   (if (:children sn)                     ;; check if node is leaf
                     (node-value sn)                      ;; if not, recur
                     (cons aggr (:meta sn))))))))))       ;; if so add meta to our aggr


(defn solve-part-2
  [puz-in]
  (let [tree      (parse-tree puz-in)
        root-node (first (tree-seq :children :children tree))]
    (->> (node-value root-node)
         flatten
         (apply +))))
