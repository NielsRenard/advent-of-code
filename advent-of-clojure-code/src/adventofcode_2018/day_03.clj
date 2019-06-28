(ns adventofcode-2018.day-03
  (:require [util :refer :all]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def input (util/get-input))

(defn clean [s]
  "parses a string like:
  \"#1 @ 483,830: 24x18\"
  into {:n 1, :x 483, :y 830, :w 24, :h 18}"
  (->> (re-seq  #"\d+" s)          ;; '("1" "483" "830" "24" "18")
       (map edn/read-string)       ;; '( 1   483   830   24   18)
       (zipmap [:n :x :y :w :h])))


(defn get-coordinates
  "Gets all the coordinates that belong to a claim."
  [{:keys [n x y w h] :as claim}]
  (for [x (range x  (+ x w))
        y (range y (+ y h))]
    {:n n :x x :y y}))


(defn sanitize-entry
  "parses a line like:
  `#1 @ 483,830: 24x18`
  into an entry `((483 830) (26 16))`."
  [line]
  (let []
    (partition 2
               (drop 1
                     (map #(edn/read-string %) ;;parse to ints
                          (-> line
                              (string/replace ":" "")
                              (string/replace "#" "")
                              (string/replace "@ " "")
                              (string/replace "," " ")
                              (string/replace "x" " ")
                              (string/split #" ")))))))


(defn entry->surface
  "takes a sanitized entry and returns all coordinates"
  [entry]
  (let [x (ffirst entry)
        y (second (first entry))
        p (first (second entry))
        q (second (second entry))]
    (for [x' (range x (if (zero? x) (+ x p) (+ x p)))
          y' (range y (if (zero? y) (+ y q) (+ y q)))]
      [x' y'])))

(defn solve-part-1
  [puz-in]
  (->> puz-in
       (map sanitize-entry)
       (map entry->surface) ;;return all claimed coordinates [[x y] [[x y] [x y]]]
       (reduce concat)      ;;flatten list [[x y][x y][x y]]
       frequencies          ;;count [[x y] 3]
       vals                 ;;             [3]
       (filter #(< 1 %))    ;;             [3]
       count))              ;;             [1]

(defn solve-part-2 [puz-in]
  "please comment and re-write this monster"
  (let [all-claims (map clean puz-in)
        claim-nums (map :n all-claims)]
    (->> all-claims
         (map get-coordinates)
         (reduce concat)
         (group-by
          (fn [c]
            {:x (:x c)
             :y (:y c)}))
         (filter
          (fn [c]
            (< 1 (count (val c)))))
         vals
         (map #(map :n %))
         flatten
         set
         (#(remove % claim-nums))
         first)))

(comment
  (def working (->> input
                    (map clean)
                    (map get-coordinates)
                    (reduce concat)
                    (group-by (fn [c] {:x (:x c) :y (:y c)}))
                    ))

  (def duplis (filter (fn [item] (< 1 (count (val item)))) working))
  (def kick-list (set (flatten (map #(map :n %) (vals duplis)))))

  (def duplicate-claim-numbers (-> (map #(->> %
                                              val
                                              (map :n)
                                              ) duplis)
                                   flatten))
  )
