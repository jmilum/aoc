(ns aoc22.day4
  (:require [babashka.fs :as fs]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.math :as math]
            [clojure.data.priority-map :refer [priority-map]]
            [util.core :as util]
            [aoc22.day3 :as day3]))

(def test-data
  ["2-4,6-8"
   "2-3,4-5"
   "5-7,7-9"
   "2-8,3-7"
   "6-6,4-6"
   "2-6,4-8"])

(defn expand [s]
  (let [limits (str/split s #"-")
        low (parse-long (first limits))
        high (inc (parse-long (second limits)))]
    (set (range low high))))

#_ (expand "2-5")

(defn expand-all [[a b]]
  (vector (expand a) (expand b)))

#_ (expand-all ["2-5" "1-8"])

(defn load-data []
  (->> (slurp "resources/aoc22/4.txt")
       (str/split-lines)
       (map #(str/split % #","))
       (map expand-all)))

#_ (load-data)

;;part 1

(defn subset-any? [[a b]]
  (or (set/subset? a b)
      (set/subset? b a)))

#_ (subset-any? (expand-all ["2-5" "1-8"]))
#_ (subset-any? (expand-all ["2-5" "3-8"]))

#_ (->> ;test-data
        (load-data)
        (map subset-any?)
        (remove false?)
        (count))

;;part 2

#_ (->> ;test-data
        (load-data)
        (map day3/intersect)
        (remove empty?)
        (count))

