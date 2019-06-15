(ns adventofcode-2017.day03
  (:require [adventofcode-2017.utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(defn nothing []
  2)

;; part 1

(defn add-side [side]
  "takes sidelength of square, returns how many tiles it takes to add a layer"
  (+ 4 (* 4 side)))

;; 1 3 5 7 9 11 13 15
;; 0 2 4 6 8 10 12 14
(def side-to-steps (sort (zipmap  (take 1000 (filter odd? (range 1000))) (take 1000 (filter even? (range 1000))))))

(def squares (sort (zipmap (range) side-to-steps)))
squares

;;Data from square 1024 must be carried 31 steps.

(def my-tile 277678)


(defn amount-of-tiles [sidelength]
  "tiles used to get side of length side"
  (+ 1 (reduce + (map add-side (filter odd? (range sidelength))))))

(def square-sides (filter odd? (range 1000)))

(defn distance-from-corner-to-center [square]
  "takes square, returns steps to get from corner to center"
  (* 2 square))


;;part two, surrounding tiles values make up next tile

;;147  142  133  122   59
;;304    5    4    2   57
;;330   10    1    1   54
;;351   11   23   25   26
;;362  747  806--->   ...

;;What is the first value written that is larger than your puzzle input?
;; x + 2|3 others > 277678

;; 1 1 2 4 5 10 11 23 25 26 54 57 59 122

(defn tiles-per-square [n]
  (map #(add-side %) (filter odd? (take n (range)))))

(def first-20-square-sizes (tiles-per-square 40))

;; have to figure out how to do sorting and zipmapping in one passthrough
;; or figure out how how to do an indexed zipmap
(defn squares-to-size-and-length [n]
  "returns a list of indexed tuples of [tiles-in-square & square-sidelength]"
  (->> (zipmap (tiles-per-square (- n 1)) (filter odd? (range n)))
       sort
       (zipmap (range))
       sort))

(defn square-size []
  (->> (filter odd? (take 100 (range)))
       (map #(* 4 %))
       (map #(- % 4))))

(defn calculate-route [squaresize]
  "from bottom-right calculates the directions to build the next square"
  (let [others (+ (/ squaresize 4) 2)
        up     (- others 1)]
    [[:right 1] [:up up] [:left others] [:down others] [:right others]]))

(defn router [n]
  "returns a list of n routing maps"
  (sort (zipmap (range 1 (+ n 1)) (map calculate-route (filter even? (take n (take-nth 8 (range))))))))

(def movelist-3
  (mapcat conj (vals (router 3))))

(defn next-tile-value [t]
  t)
