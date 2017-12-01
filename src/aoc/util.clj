(ns aoc.util
  (:require
   [clojure.main]
   [clojure.java.io :as jio]
   [clojure.set :as set]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]
   [clojure.math.combinatorics :as combo]
   [clojure.algo.generic.functor :as f]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro defmethod*
  "closure over defmethod to bind the dispatch-val with :as"
  [multifn dispatch-val & fn-tail]
  (let [[kw n & body] fn-tail]
    (if (= :as kw)
      `(let [~n ~dispatch-val]
         (defmethod ~multifn ~dispatch-val ~body))
      `(defmethod ~dispatch-val ~fn-tail))))

(defmacro when-let*
  "allow multiple bindings in when-let"
  ([bindings & body]
   (if (seq bindings)
     `(when-let [~(first bindings) ~(second bindings)]
        (when-let* ~(drop 2 bindings) ~@body))
     `(do ~@body))))

(defmacro if-let*
  "allow multiple bindings in if-let"
  ([bindings then]
   `(if-let* ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(if-let [~(first bindings) ~(second bindings)]
        (if-let* ~(drop 2 bindings) ~then ~else)
        ~(if-not (second bindings) else))
     then)))

(defmacro ->map
  "create a map of the values with the names as keywords"
  [& ks]
  (zipmap (map keyword ks) ks))

(defn map-kv [f coll] (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defn str->int [s] (when s (Integer/parseInt s)))

(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit ^Character %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit ^Character %) s)]
      (empty? s))))

(defn save-coll
  [file coll]
  (when (seq coll)
    (->> coll
         (interpose \newline)
         (apply str)
         (spit file))))

(defn third [coll] (nth coll 2))
(defn fourth [coll] (nth coll 3))
(defn fifth [coll] (nth coll 4))
(defn sixth [coll] (nth coll 5))
(defn seventh [coll] (nth coll 6))
(defn eighth [coll] (nth coll 7))
(defn ninth [coll] (nth coll 8))
(defn tenth [coll] (nth coll 9))

(defn replace-several
  [str & replacements]
  (reduce (fn [s [a b]]
              (str/replace s a b))
          str
          (partition 2 replacements)))

(defn avg-rd [avg i x] (float (+ avg (/ (- x avg) (inc i)))))

(defn reduce-xf [f init]
  (fn [rf]
      (let [acc (volatile! init)]
        (completing (fn [result input] (rf result (vswap! acc f input)))))))

(defn round-double [n x] (->> x (double) (format (str "%." n "g")) (Double/parseDouble)))

(defn round-dec [n x]
  (Double/parseDouble (format (str "%." n "f") x)))

(defn unchunk [s]
  (when (seq s)
    (lazy-seq
     (cons (first s)
           (unchunk (next s))))))

