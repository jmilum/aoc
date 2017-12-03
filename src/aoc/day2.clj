(ns aoc.day2
  (:require
   [clojure.main]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]
   [clojure.math.combinatorics :as combo]
   [clojure.algo.generic.functor :as f]
   [aoc.util :as util]
   [clojure.string :as string]))

(defn file->digits [file]
  (-> file
      slurp
      (string/replace #"\t" " ")
      string/split-lines
      (->> (map #(string/split % #" "))
           (f/fmap #(map util/str->int %)))))

(defn max-min [coll] (- (apply max coll) (apply min coll)))

#_(transduce (map max-min) + (file->digits "resources/day2.txt"))

(defn divis [coll]
  (->> (combo/combinations coll 2)
       (map #(/ (apply max %) (apply min %)))
       (filter integer?)))

#_(transduce (mapcat divis) + (file->digits "resources/day2.txt"))

