(ns adventofcode-2017.day07
  (:require [adventofcode-2017.utils :as utils]
            [clojure.string :as string]))

(def puz-in (into [] (utils/read-all-lines "resources/2017/07.txt")))



(defn split-up [s]
  (string/split s #" "))

(def weight-of-whole-tower (reduce + (map #(read-string (string/replace % #"[^0-9]" "")) (map second (map split-up puz-in)))))

(defn get-all-supporting [input]
  (map #(first (split-up %)) input))

(defn parse-supported [s]
  (into [] (map utils/remove-commas (drop 1 (drop-while #(not (= "->" %)) s)))))

(defn get-all-supported [input]
  (filter not-empty (map #(parse-supported(split-up %)) input )))


(def all-supporting (get-all-supporting puz-in))
(def all-supported (map utils/remove-commas(apply concat (get-all-supported puz-in))))

;;check which of the programs is not in the list of supported programs
;;answer to 1

(def solution-part-one (clojure.set/difference (set all-supporting) (set all-supported)))

(def bottom-program :hmvwl)


;;part 2


(def string->map
  (fn [string]
    (let [s           (split-up string)
          supports    (parse-supported s)
          ]
      (hash-map (keyword (first s))
                (hash-map :options (merge {:weight (read-string(string/replace (second s) #"[^0-9]" ""))
                                           :name (first s)}
                                          (when-let [supporting? (= "->" (nth s 2 nil))] {:supports supports} )))))))

(defn aggr-map "creates a readable map of the puzzle-input" [input] (reduce merge (map string->map input)))
(def am (aggr-map puz-in))

(defn lookup-node "pulls node from map"
  ([aggr-map node-key]
   (get aggr-map node-key))
  ([node-string]
   (let [am (aggr-map puz-in)]
     (get am (keyword node-string)))))

(defn node->supports "shows which nodes a node supports"
  ([aggr-map node-key]
   (get-in (lookup-node aggr-map node-key) [:options :supports]))
  ([node-string]
   (let [am (aggr-map puz-in)]
     (get-in (lookup-node am (keyword node-string)) [:options :supports]))))

(defn get-all-weights "ugly function that gets the total weight of all programs" [m]
  (map #(string/replace % #"[^0-9]" "") (map #(get-in (second %) [:options :weight]) m)))

(defn top-level? "helps " [m]
  (let [name (first m)
        opts (get (second m) :options)]
    (nil? (get opts :supports))))

(def all-top-levels
  (filter top-level? (aggr-map puz-in)))

(defn get-carried-weight "returns the weight of the nodes it supports"
  [node-key]
  (let [node     (lookup-node node-key)
        supports (map lookup-node (node->supports node-key))
        opts     (:options node)]
    (map #(merge {:name (:name (:options %))}
                 {:weight (get (:options %) :weight)}) supports)))

(defn get-supported-weights [node]
  (map lookup-node (node->supports node)))

(defn show-balance [node]
  "takes a node and returns the weight of its supportees"
  (map #(get-in % [:options :weight]) (get-supported-weights node)))

(def temp-atom (atom 0))

(defn climb-tree "takes a (starting) node and starts climbing up the tree"
  [node]
  (let [right-above (node->supports node)
        weight-acc  (reduce + (show-balance node))
        a           temp-atom]
    (do (swap! a (partial + weight-acc))
        (if (some? right-above)
          (map #(climb-tree %) right-above)
          (prn @temp-atom)))))

;; I ran climb-tree on each of the five towers to find which one was off
(def nswximo-tot (+ 65478 36294))
(def tpphe-tot (+ 64887 36885))
(def nswximo-baewpe (+ 55 101717))
(def hghnmib-tot (+ 47802 53970))
(def kzltfq-tot (+ 94288 7490))  ;; this one is too high by six

;;repeat for nodes in that tower
;;answer was arqoys node with weight 1859, needed to lose 6;
