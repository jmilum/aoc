(ns aoc22.day3
  (:require [babashka.fs :as fs]
            [clojure.data.csv :as csv]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [babashka.deps :as deps]
            [util.core :as util]))

(def test-data
  ["vJrwpWtwJgWrhcsFMMfFFhFp"
   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
   "PmmdzqPrVvPwwTWBwg"
   "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
   "ttgJtRGJQctTZtZT"
   "CrZsJsPPZsGzwwsLwLmpwMDw"])

(defn load-data []
  (->> (slurp "resources/aoc22/3.txt")
       (str/split-lines)))

;part 1

(defn half-seq [coll]
  (let [n (count coll)]
    (split-at (/ n 2) coll)))

(defn intersect [[a b]]
  (set/intersection (set a) (set b)))

(defn char-uppercase? [c]
  (= (str c) (str/upper-case (str c))))

(defn priority [c]
  (if (char-uppercase? c)
    (- (int c) 38)
    (- (int c) 96)))

(->> (load-data)
     (map seq)
     (map half-seq)
     (map intersect)
     (map first)
     (map priority)
     (reduce +))

;part 2

(defn intersect-group [[a b c]]
  (set/intersection (set a) (set b) (set c)))

(->> (load-data)
     (map seq)
     (partition 3)
     (map intersect-group)
     (map first)
     (map priority)
     (reduce +))

