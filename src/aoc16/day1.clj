(ns advent.day1
  (:require
    [clojure.edn :as edn]
    [clojure.math.numeric-tower :as math]
    [advent.util :as util :refer [->map]]))

(def start {:done false :orient :N :position {:x 0 :y 0} :log #{{:x 0 :y 0}}})
(def data (map str (edn/read-string (slurp "resources/day1.edn"))))

(defn rotate [state turn]
  (condp = (:orient state)
    :N (assoc state :orient (if (= :L turn) :W :E))
    :S (assoc state :orient (if (= :L turn) :E :W))
    :E (assoc state :orient (if (= :L turn) :N :S))
    :W (assoc state :orient (if (= :L turn) :S :N))))

(defn move-step [op coord state _]
  (let [new (update-in state [:position coord] op)]
    (if ((:log state) (:position new))
      (reduced (assoc new :done true))
      (update new :log conj (:position new)))))

(defn move-positions [state distance]
  (let [op (if (#{:N :E} (:orient state)) inc dec)
        coord (if (#{:N :S} (:orient state)) :y :x)]
    (reduce #(move-step op coord %1 %2) state (range distance))))

(defn walk [state leg]
  (let [turn     (keyword (str (first leg)))
        distance (util/str->int (apply str (rest leg)))
        steps (-> state (rotate turn) (move-positions distance))]
    (if (:done steps) (reduced steps) steps)))

(let [end (reduce walk start data)
      {x-start :x y-start :y} (:position start)
      {x-end :x y-end :y} (:position end)]
  (+ (math/abs (- x-start x-end))
     (math/abs (- y-start y-end))))





