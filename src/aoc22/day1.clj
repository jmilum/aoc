(ns aoc22.day1
  (:require [babashka.fs :as fs]
            [clojure.data.csv :as csv]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [babashka.deps :as deps]))

(defmacro dbg [body]
  `(let [x# ~body]
    (println '~body "=")
    (println x#
      x#)))

#_ (+ 1 2 3)

(defn sum [coll]
  (->> coll
       (map parse-long)
       (reduce +)))

(defn load-data [] (-> (slurp "resources/aoc22/1.txt")
                       (str/split #"\n\n")
                       (->> (map str/split-lines)
                            (map sum))))

;part 1
#_ (->> (load-data)
        (apply max))


;part 2
#_ (->> (load-data)
        (sort >)
        (take 3)
        (reduce +))

