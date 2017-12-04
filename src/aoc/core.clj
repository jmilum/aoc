(ns aoc.core
  (:require
   [clojure.main]
   [clojure.set :as set]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]
   [clojure.math.combinatorics :as combo]
   [clojure.algo.generic.functor :as f]
   [aoc.util :as util]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn n->xy [n]
  (let [n (dec n) ; start from 1
        ring (int (math/floor (/ (inc (math/sqrt n)) 2)))
        start (math/expt (dec (* 2 ring)) 2)
        length (* 8 ring)
        quadrent (int (math/floor (* 4 (- n start) (/ length))))
        offset (- n start (math/floor (* 0.25 quadrent length)))
        coord2 (int (- (inc offset) ring))]
    (case quadrent
      0 [ring coord2]        ; north
      1 [(- coord2) ring]        ; east
      2 [(- ring) coord2]    ; south
      3 [coord2 (- ring)]))) ; west

(defn manh-dist [[x y]] (+ (math/abs x) (math/abs y)))

(defn eucl-dist [[x y]] (math/sqrt (+ (* x x) (* y y))))

#_ (manh-dist (n->xy 347991))

(defn xy->quad [[x y]]
  (let [x' (>= x 0)
        y' (>= y 0)]
    (condp = [x' y']
      [true true]  0
      [true false] 1
      [false false] 2
      [false true] 3)))

(defn xy->n [[x y]]
  (let [ring (apply max (map math/abs [x y]))
        start (math/expt (dec (* 2 ring)) 2)
        quad (xy->quad [x y])
        length (* 8 ring)
        offset (+ (math/abs x) (math/abs y) (- 1))
        n (+ 1 start offset (* 0.25 quad length))]
    (util/->map ring start quad offset n)))



;(defn xy->n [[x y]]
;  (let [abs-x (math/abs x)
;        abs-y (math/abs y)
;        a (max abs-x abs-y)
;        b (if (> x y) (+ x y) (- x y))
;        c (+ 1 (math/abs b) (* 4 a a))
;        d (* 2 a)]
;    (if (> abs-x abs-y)
;      (if (> x 0)
;        (- c d d)
;        c)
;      (if (> y 0)
;        (- c d)
;        (+ c d)))))



(defn n->adjacent-xy [n]
  (let [[x y] (n->xy n)]
    [[x (inc y)]
     [(inc x) (inc y)]
     [(inc x) y]
     [(inc x) (dec y)]
     [x (dec y)]
     [(dec x) (dec y)]
     [(dec x) y]
     [(dec x) (inc y)]]))

(n->xy 2)
(n->adjacent-xy 2)
;(map xy->n (n->adjacent-xy 25))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day2-data []
  (-> "resources/day2.txt"
      slurp
      (str/replace #"\t" " ")
      str/split-lines
      (->> (map #(str/split % #" "))
           (f/fmap #(map util/str->int %)))))

(defn max-min [coll] (- (apply max coll) (apply min coll)))

#_(transduce (map max-min) + (day2-data))

(defn divis [coll]
  (->> (combo/combinations coll 2)
       (map #(/ (apply max %) (apply min %)))
       (filter integer?)))

#_(transduce (mapcat divis) + (day2-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; day 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn day1-data []
 (->> "resources/day1.txt"
      slurp
      str/trim
      seq
      (map str)
      (mapv util/str->int)))

(defn invcap1 []
 (let [digits (day1-data)
       data (conj digits (first digits))]
   (->> (partition 2 1 data)
        (map (fn [[a b]] (if (= a b) a 0)))
        (reduce +))))

#_ (invcap1)

(defn invcap2 []
  (let [digits (day1-data)
        halfway (/ (count digits) 2)
        buffer (concat (drop halfway digits) (take halfway digits))
        data (interleave digits buffer)]
   (->> (partition 2 data)
        (map (fn [[a b]] (if (= a b) a 0)))
        (reduce +))))

#_(invcap2)



