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

(defn generate-coordinates
  [area]
  (for [y (range (count area))
        x (range (count area))]
    {:x x :y y}))

(defn attach-coordinates [area]
  (map (fn [plot {:keys [x y]}] {:x x :y y :plot plot}) (flatten area) (generate-coordinates area)))

(defn lmao-vectored-map [coordinated-map]
  "refactor me in the morning please"
  (map #(into [] %) (partition 50 (map vector coordinated-map)))
  )

(defn ugh [res]
  (map #(into [] (map vector %)) res))

(defn step-through [lmao]
  (for [row lmao]
    (map #(transform-if-allowed lmao %) row))
  )

(defn solve-part-1 [input]
  (-> (parse-area input)
      (attach-coordinates)
      (lmao-vectored-map)
      step-through
      ugh
      step-through
      ugh
      step-through
      ugh
      step-through
      ugh
      step-through
      ugh
      step-through
      ugh
      step-through
      ugh
      step-through
      ugh
      step-through
      ugh
      step-through
      print-result
      ))


(defn print-result [coordinated-map]
  (->> coordinated-map
       (map #(map :plot %))
       (print-grid)))


(comment
  print-grid (map #(map :plot %)
                  ))

(defn- get-one-acre [grid {:keys [x y]}]
  (if (and (< -1 x 50) (< -1 y 50))
    (:plot (first (nth (nth grid y) x)))
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
    \. \|    ;; '.' becomes '|' if 3 or more adjacent are |
    \| \#    ;; '|' becomes '#' if 3 or more adjacent are |
    \# \.    ;; '#' stays '#' if 1 adjacent '|' && 1 adjacent '#'
    nil       ;; else becomes '.'
    ))

(defn transform-if-allowed [grid [{:keys [x y plot] :as acre}]]
  (let [adjacents          (frequencies (get-adjacents grid acre))
        three-trees?       (if (some? (get adjacents \|))
                             (< 2 (get adjacents \|))
                             false)
        three-lumberyards? (if (some? (get adjacents \#))
                             (< 2 (get adjacents \#))
                             false)
        one-of-each?       (every? pos-int? [(get adjacents \|) (get adjacents \#)])]

    {:x x :y y :plot (cond
                       (and (= plot \.) three-trees?) (transform plot)
                       (and (= plot \|) three-lumberyards?) (transform plot)
                       (and (= plot \#) (not one-of-each?)) (transform plot)
                       :else plot)}
    ))

(defn run-a-step [area]
  (reduce (fn [grid acre]
            (transform-if-allowed grid acre))
          area))

(defn run-one-step [area]
  (map #(map transform %) (parse-area area)))

(comment
  ;;answer to 1
  (->> example-input
       solve-part-1
       step-through
       ugh
       step-through
       ugh
       step-through
       ugh
       step-through
       ugh
       step-through
       ugh
       step-through
       ugh
       step-through
       ugh
       step-through
       ugh
       step-through
       ugh
       step-through
       print-result)

  (map #(map :plot %) (for [row coordimap]
                        (map #(transform-if-allowed coordimap %) row)))

  ;;50x50 grid of whatever
  (vec (repeat 50 (vec (repeat 50 (cycle ["." "|" "#"])))))
  (get-adjacents (repeat 50 (take 50 (cycle ["." "|" "#"]))) {:x 1 :y 1})

  ;; attach coords to one-line
  (defn attach-coordinates [area]
    (let [coords (for [x (range 10)]
                   {:x x :y 0})]
      (map (fn [line {:keys [x y]}] {:plot line :x x :y y}) area coords)))

  )
