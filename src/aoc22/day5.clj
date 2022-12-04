(ns aoc22.day5
  (:require [babashka.fs :as fs]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.math :as math]
            [clojure.data.priority-map :refer [priority-map]]
            [util.core :as util]
            [aoc22.day3 :as day3]))

(def test-data)

(defn load-data []
  (->> (slurp "resources/aoc22/5.txt")
       (str/split-lines)))

#_ (load-data)

;;part 1

#_ (->> test-data)
        ;(load-data))

;;part 2
