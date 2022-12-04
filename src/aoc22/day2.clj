(ns aoc22.day2
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math :as math]))

;; part1

(def actions
  {:A :rock
   :B :paper
   :C :scis
   :X :rock
   :Y :paper
   :Z :scis})

(def outcome
  {[:rock :scis]   :lose
   [:rock :paper]  :win
   [:rock :rock]   :draw
   [:scis :paper]  :lose
   [:scis :rock]   :win
   [:scis :scis]   :draw
   [:paper :rock]  :lose
   [:paper :scis]  :win
   [:paper :paper] :draw})

(def scores
  {:win   6
   :lose  0
   :draw  3
   :rock  1
   :paper 2
   :scis  3})

(def test-data [[:rock :paper] [:paper :rock] [:scis :scis]])

(defn load-data [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(str/split % #" "))
       (map #(vector (actions (keyword (first %)))
                     (actions (keyword (second %)))))))

#_ (->> (load-data "resources/aoc22/2.txt")
        (mapcat #(vector (second %) (outcome %)))
        (map scores)
        (apply +))

;; part 2

(def test-data2 [[:rock :draw] [:paper :lose] [:scis :win]])

(def actions2
  {:A :rock
   :B :paper
   :C :scis
   :X :lose
   :Y :draw
   :Z :win})

(def outcome2
  {[:rock :win]   :paper
   [:rock :lose]  :scis
   [:rock :draw]  :rock
   [:scis :win]   :rock
   [:scis :lose]  :paper
   [:scis :draw]  :scis
   [:paper :win]  :scis
   [:paper :lose] :rock
   [:paper :draw] :paper})

(defn load-data2 [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(str/split % #" "))
       (map #(vector (actions2 (keyword (first %)))
                     (actions2 (keyword (second %)))))))

#_ (->> (load-data2 "resources/aoc22/2.txt")
        (mapcat #(vector (second %) (outcome2 %)))
        (map scores)
        (apply +))


