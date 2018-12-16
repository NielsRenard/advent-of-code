(ns adventofcode-2018.day-11
  (:require [util :refer :all]
            [clojure.edn :as edn]))

(def input 3999)

;;cut this down
(defn find-power-level
  [[serial x y]]
  (let [rack-id      (+ x 10)
        power-level (->> rack-id
                         (* y)
                         (+ serial)
                         (* rack-id)
                         str
                         (take-last 3)
                         first
                         str
                         edn/read-string
                         )]
    (- power-level 5)))

(defn get-power-levels
  [serial size]
  (for [x (range 0 size)
        y (range 0 size)]
    {:x x :y y :pl (find-power-level [serial x y])}))


(defn- blank-canvas [size]
  (vec (repeat size (vec (repeat size "_")))))

(defn- put-one-in-grid [canvas {:keys [x y pl]}]
  (assoc-in canvas [x y] pl))


;;not necessary but fun to print grids
(defn print-grid [canvas]
  (->> canvas
       (map clojure.string/join)
       (clojure.string/join "\n")
       println))


(defn generate-filled-grid [serial size]
  (let [new-canvas   (blank-canvas size)
        power-levels (get-power-levels serial size)]
    (reduce (fn [canvas x] (put-one-in-grid canvas x))
            new-canvas
            power-levels)))

;; not sure if this works with 0 coords
;; but we just leave an empty column/row at 0,0
(defn calc-subgrid-coords [size {:keys [x y] :as coord}]
  "Returns the coords for a size-x-size grid (starting at the left-top.)"
  (for [x (range x (+ x size))
        y (range y (+ y size))]
    {:x x :y y}))

(defn- get-one-coord-val [grid {:keys [x y]}]
  (nth (nth grid x) y))

(defn- get-multi-coord-vals [grid coords]
  (->> coords
       (map #(get-one-coord-val grid %) ,,,)))


(defn sum-all-vals [grid]
  (reduce + (flatten grid)))


(defn sum-a-sub-grid [grid subsize {:keys [x y] :as coord}]
  (->> (calc-subgrid-coords subsize coord)
       (get-multi-coord-vals grid ,,,)
       (sum-all-vals)))


(defn solve-part-1
  [serial]
  (let [fg (generate-filled-grid serial 300)]
    (->> (for [x (range 100)                            ;; get all the subgrids (now just first 100)
               y (range 100)]
           {:coords {:x x :y y}
            :sum    (sum-a-sub-grid fg 3 { :x x :y y})})  ;; get the sums of all the subgrids
         (sort-by :sum)                                 ;; sort
         last                                           ;; biggest one, maybe use max-key here?
         :sum)))

(comment
  ;;to get one subgrid from a filled-grid ww
  (get-multi-coord-vals ww (calc-subgrid-coords 3 {:x 33 :y 45}))

  ;;assoc in grid (vec of vecs)
  (def ex1 (assoc-in (blank-canvas 10) [2 2] "2"))
  (print-grid ex1)
  ;;__________
  ;;__________
  ;;__2_______
  ;;__________
  ;;__________
  ;;__________
  ;;__________
  ;;__________
  ;;__________
  ;;__________
  )
