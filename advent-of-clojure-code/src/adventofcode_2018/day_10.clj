(ns adventofcode-2018.day-10
  (:require [util :refer :all]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def input (util/get-input))

(defn parse-line [line]
  "parses this string:
   position=< 9,  1> velocity=< 0,  2>
   to this
   {:x 9, :y 1, :dx 5, :dy 1}"
  (zipmap (list :x :y :dx :dy) (map edn/read-string (re-seq #"-?\d+" line))))


(defn translate
  "Takes a map of x & y coordinates and their velocities, calculates
   the next position. Goes forward, or backward when 2nd arg truthy.
   {:x 9,  :y 1, :dx 5, :dy 1} becomes
   {:x 14, :y 2, :dx 5, :dy 1}."
  [{:keys [x y dx dy]} & backward?]
  (if backward?
    {:x (- x dx) :y (- y dy) :dx dx :dy dy}
    {:x (+ x dx) :y (+ y dy) :dx dx :dy dy}))


(defn parse-data [puz-in]
  (->> puz-in
       (map parse-line)))


(defn measure-canvas
  "Gets the minimum and maximum coordinate values to determine
   how large the canvas is."
  [parsed-data]
  (let [sorted-by-x (sort-by :x parsed-data)
        sorted-by-y (sort-by :y parsed-data)
        x-min       ((comp :x first) sorted-by-x)
        x-max       ((comp :x last) sorted-by-x)
        y-min       ((comp :y first) sorted-by-y)
        y-max       ((comp :y last) sorted-by-y)        ]
    ;;I feel like this could be shorter somehow
    ;;something something juxt identity keyword
    #_{:x-min x-min :y-min y-min :width (- x-max x-min) :height (- y-max y-min)}
    [x-min y-min (- x-max x-min) (- y-max y-min)]))


;; this fn tries to find the smallest canvas
;; the idea is that the right moment is right before the stars start to 'expand' again.
(defn recur-translate-measure
  "find the most compact canvas"
  ([parsed-data] (recur-translate-measure parsed-data 300000 0))
  ([parsed-data canvas-size n]
   (let [new-canvas-size (-> (measure-canvas parsed-data)
                             vals
                             (#(reduce + %)))]
     (if (< new-canvas-size canvas-size) ;;compare size of canvas each run
       (recur (->> parsed-data
                   (map translate)) new-canvas-size (inc n))
       (map #(translate % true) parsed-data))))) ;; translate backward once because


(defn make-stars [parsed-data]
  (let [[x-min y-min width height] (measure-canvas parsed-data)
        nightsky (vec (repeat 20 (vec (repeat 80 "_"))))]
    (reduce
     (fn [canvas {:keys [x y]}]
       (assoc-in canvas [(- y y-min) (- x x-min)] "|"))
     nightsky
     parsed-data)))

(defn print-stars [stars]
  (println (string/join "\n" (map string/join stars))))


(comment
  ;;this works for example, but blows the stack on input.
  (print-raster (rasterize (recur-translate-measure (parse-data example-input))))
  ;;alternate translate
  (-> coord
      (assoc :x (+ x dx))
      (assoc :y (+ y dy))))
