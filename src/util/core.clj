(ns util.core
  (:require [clojure.string :as str]
            [clojure.java.io :as jio]
            [clojure.set :as set]
            [clojure.data.csv :as csv]
            [clojure.string :as str]))


(defmacro dbg [body]
  `(let [x# ~body]
    (println '~body "=")
    (println x#
      x#)))

;;; Parsing

(defn str-to-coll-base
  "Convert a string into a collection, reading every character in the
  given base; e.g, \"12312094\" -> '(1 2 3 1 2 0 9 4) in base 10."
  [b s]
  (map #(Character/digit ^char % ^int b) s))

(defn coll-to-base
  "Convert a collection a number in the given base; e.g.,
  '(1 2 3 1 2 0 9 4) -> `12312094' in base 10."
  [b xs]
  (BigInteger. ^String (apply str xs) ^int b))

(defn words [s]
  (str/split s #" "))

(defn split-groups [s]
  (str/split s #"\n\n"))

;;; Pretty printing

(defn plot-set
  "Plot a set of points of the form [x y]; ASCII style."
  [s]
  (let [[xmax ymax] (reduce (fn [[x-max y-max] [x y]]
                              [(max x-max x) (max y-max y)])
                            s)
        [xmin ymin] (reduce (fn [[x-min y-min] [x y]]
                              [(min x-min x) (min y-min y)])
                            s)]
    (map (fn [line] (str/join
                     (map (fn [[a b]] (if (s [a b]) "█" " "))
                          line)))
         (map (fn [y] (map #(vector % y)
                           (range xmin (inc xmax))))
              (range ymin (inc ymax))))))

;;; Stuff that should be in clojure.core

(defn sum [xs]
  (reduce + xs))

(defn transpose [mat]
  (apply mapv vector mat))

(defn elem [x xs]
  (some #{x} xs))

(defn permutations [[h & t :as coll]]
  (if (nil? t)
    [coll]
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(defn converge
  "Apply a function `f' until it converges."
  [f xs]
  (let [xss (f xs)]
    (if (= xss xs) xs (recur f xss))))

(defn converge-when
  "Apply a function `f' until it converges and count how long that takes."
  ([f xs]   (converge-when f xs 0))
  ([f xs n]
   (let [xss (f xs), m (inc n)]
     (if (= xss xs) [m xs] (recur f xss m)))))

(defn map-val
  "Map over the values of a given map."
  [f hmap]
  (into {} (map (fn [[k v]] {k (f v)})) hmap))

(defn filter-val
  "Filter a map by applying `f' to its values."
  ([f]      (filter #(f (val %))))
  ([f hmap] (filter #(f (val %)) hmap)))

(defn map-from-coll-with
  "Turn a collection into a map.  First map `g' over every element to
  produce a map and then merge the resulting maps, applying `f' in case
  of duplicate keys.  For example:

    (map-from-coll-with + #(hash-map % 1) [1 2 3 1]) ≡ {1 2, 3 1, 2 1}."
  [f g coll]
  (apply merge-with f (map g coll)))

(defn map-from-coll
  "Like `map-from-coll-with', but ignore the second value in case of a
  conflict."
  [g coll]
  (map-from-coll-with (fn [a _] a) g coll))

;;; Matrix manipulation

(defn mat-ix
  "Return the element at index (i, j).  Returns `nil' if index is
  out-of-bounds."
  [m [i j]]
  (let [rows (count m)
        cols (count (first m))]
    (when (and (< -1 i rows) (< -1 j cols))
      (nth (nth m i) j))))

(defn map-matrix
  "Map a function f(i, j, el) over all elements of a matrix with
  indices."
  [f mat]
  (apply concat
         (keep-indexed (fn [i row]
                         (keep-indexed (fn [j el]
                                         (f i j el))
                                       row))
                       mat)))

;; old util stuff

(defmacro doseq-indexed
  "loops over a set of values, binding index-sym to the 0-based index of each value"
  ([[val-sym values index-sym] & code]
   `(loop [vals# (seq ~values)
           ~index-sym (long 0)]
      (if vals#
        (let [~val-sym (first vals#)]
          ~@code
          (recur (next vals#) (inc ~index-sym)))
        nil))))

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

(defn round-double [n x] (->> x (double) (format (str "%." n "g")) (parse-double)))

(defn unchunk [s]
  (when (seq s)
    (lazy-seq
     (cons (first s)
           (unchunk (next s))))))

(defn drop-take [coll m]
  (let [start  (get m :start 0)
        amount (get m :amount Integer/MAX_VALUE)
        stop   (min amount (get m :stop Integer/MAX_VALUE))]
    (->> coll (drop start) (take stop))))

(def fib-seq-seq
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a b)))))
   0 1))

(defn pad-to-n [n x v]
  (into v (repeat (- n (count v)) x)))

(defn pad [n coll val]
  (take n (concat coll (repeat val))))

(defn lump [& colls]
  "Column major to row major"
  (partition
   (count colls)
   (apply interleave colls)))

(defn unlump [coll]
  "Row major to column major"
  (map
   #(map
     (fn [i] (nth coll i) %)
     (range (count (first coll))))
   coll))
