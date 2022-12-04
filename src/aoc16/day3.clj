(ns advent.day3
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clojure.math.numeric-tower :as math]
    [advent.util :as util :refer [->map]]))

(def data (->> (slurp "resources/day3.edn")
               (edn/read-string)
               (partition 3)
               (apply interleave)
               (partition 3)))

(defn possible-triangle? [[a b c]]
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(frequencies (map possible-triangle? data))



;(defn data [] (->> (slurp "resources/day3.edn")
;                   (edn/read-string)
;                   (partition 3)))
;
;(defn possible-triangle? [[a b c]]
;  (and (> (+ a b) c)
;       (> (+ a c) b)
;       (> (+ b c) a)))
;
;(frequencies (map possible-triangle? (data)))
