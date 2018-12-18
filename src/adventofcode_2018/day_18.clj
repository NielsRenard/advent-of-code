(ns adventofcode-2018.day-18
  (:require [util :refer :all]
            [clojure.string :as string]))

(def input (util/get-input))

(def example-input
  '(".#.#...|#."
    ".....#|##|"
    ".|..|...#."
    "..|#.....#"
    "#.#|||#|#|"
    "...#.||..."
    ".|....|..."
    "||...#|.#|"
    "|.||||..|."
    "...#.|..|."))

(defn print-grid [canvas]
  (->> canvas
       (map clojure.string/join)
       (clojure.string/join "\n")
       println))

(defn parse-area [input]
  (->> input
       (map seq)))


(defn- get-one-acre [grid {:keys [x y]}]
  (if (and (< -1 x 49) (< -1 y 49))
    (nth (nth grid x) y)
    nil))


(defn get-adjacents [grid {:keys [x y]}]
  "Returns the 8 adjacent acres for an acre."
  (let [left   {:x (dec x) :y y}
        right  {:x (inc x) :y y}
        toprow [{:x (dec x) :y (dec y)}
                {:x x :y (dec y)}
                {:x (inc x) :y (dec y)}]
        botrow [{:x (dec x) :y (inc y)}
                {:x x :y (inc y)}
                {:x (inc x) :y (inc y)}]]

    (map #(get-one-acre grid %) (flatten [toprow left right botrow]))))


(defn transform [char]
  (condp = char
    \. "|"    ;; '.' becomes '|' if 3 or more adjacent are |
    \| "#"    ;; '|' becomes '#' if 3 or more adjacent are |
    \# "."    ;; '#' stays '#' if 1 adjacent '|' && 1 adjacent '#'
    nil       ;; else becomes '.'
    ))

(defn transform-if-allowed [grid {:keys [x y plot] :as acre}]
  (let [adjacents          (frequencies (get-adjacents grid acre))
        three-trees?       (< 2 (get adjacents "|"))
        three-lumberyards? (< 2 (get adjacents "#"))
        one-of-each?       (every? pos-int? (vals (drop 1 adjacents)))]

    (cond
      (and (= plot \.) three-trees?) (transform plot)
      (and (= plot \|) three-lumberyards?) (transform plot)
      (and (= plot \#) (not one-of-each?)) (transform plot)
      :else plot)
    ))

(defn run-a-step [area]
  ;;get-adjacents from area
  ;;check against value (cond)
  (reduce (fn [grid acre]
            (transform-if-allowed grid acre))
          area))



(defn run-one-step [area]
  (map #(map transform %) (parse-area area)))

(comment
  (get-adjacents (repeat 50 (take 50 (cycle ["." "|" "#"]))) {:x 1 :y 1})
  )
