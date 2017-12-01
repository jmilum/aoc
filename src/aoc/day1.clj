(ns aoc.day1
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

(def file-data1 "resources/day1.txt")

(defn file->digits [file]
 (->> file
      slurp
      str/trim
      seq
      (map str)
      (mapv util/str->int)))

(defn invcap1 []
 (let [digits (file->digits file-data1)
       data (conj digits (first digits))]
   (->> (partition 2 1 data)
        (map (fn [[a b]] (if (= a b) a 0)))
        (reduce +))))

(defn invcap2 []
  (let [digits (file->digits file-data1)
        halfway (/ (count digits) 2)
        buffer (concat (drop halfway digits) (take halfway digits))
        data (interleave digits buffer)]
   (->> (partition 2 data)
        (map (fn [[a b]] (if (= a b) a 0)))
        (reduce +))))

