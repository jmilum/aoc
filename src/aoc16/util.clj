(ns advent.util
  (:require
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [clojure.set :as set]
    [clojure.string :as str])
  (:import (java.util Date UUID)
           (java.io File FileNotFoundException Reader Writer BufferedReader)
           (java.sql SQLException)
           (clojure.lang BigInt Ratio)))

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
  "create a map of the values with the var names as keywords"
  [& kv]
  (zipmap (map keyword kv) kv))

(defmacro ->map-ns
  "create a map of the values with the var names as namespaced keywords with the given namespace"
  [ns-str & kv]
  (zipmap (map #(keyword ns-str (str %)) kv) kv))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn third   [coll] (nth coll 2))
(defn fourth  [coll] (nth coll 3))
(defn fifth   [coll] (nth coll 4))
(defn sixth   [coll] (nth coll 5))
(defn seventh [coll] (nth coll 6))
(defn eighth  [coll] (nth coll 7))
(defn ninth   [coll] (nth coll 8))
(defn tenth   [coll] (nth coll 9))

(defn lazy-file-lines [file]
  (letfn [(helper [^BufferedReader rdr]
            (lazy-seq
              (if-let [line (.readLine rdr)]
                (cons line (helper rdr))
                (do (.close rdr) nil))))]
    (helper (io/reader file))))

(defn get-edn-lines [path] (map edn/read-string (lazy-file-lines path)))

(defn uuid [] (.toString (UUID/randomUUID)))

(defn map-kv [f coll] (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defn str->int [s] (when s (Integer/parseInt s)))

(defn group-by-with-transducer [key-f xf data]
  (let [ff (memoize (fn [k] (xf conj)))
        r  (reduce (fn [m d]
                     (let [k (key-f d)
                           v (get m k [])]
                       (if (reduced? v)
                         m
                         (assoc m k ((ff k) v d)))))
                   data)]
    (into {} (map (fn [[k v]] [k ((ff k) (unreduced v))])) r)))

(defn dir-file-list
  [directory name-pattern]
  (->> directory
       (io/file)
       (file-seq)
       (filter #(re-find name-pattern (.getName %)))))

(defn dups [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))

(defn duplicates
  [data params]
  (->> data
       (map params)
       frequencies
       (filter #(> (int (second %)) 1))
       (map first)
       set))

(defn numeric? [^String s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit ^char %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit ^char %) s)]
      (empty? s))))

(defn symmetric-difference [s1 s2]
  (set/union (set/difference s1 s2) (set/difference s2 s1)))

(defn save-coll
  [file coll]
  (when (seq coll)
    (->> coll
         (interpose \newline)
         (apply str)
         (spit file))))

(defn copy-file [source dest] (io/copy (io/file source) (io/file dest)))

(defn safe-delete [^String file-path]
  (if (.exists (File. file-path))
    (try
      (io/delete-file file-path)
      (catch Exception e (str "exception: " (.getMessage e))))
    false))

(defn delete-directory-contents [directory-path]
  (let [directory-contents (file-seq (io/file directory-path))
        files-to-delete    (filter #(.isFile ^File %) directory-contents)]
    (doseq [file files-to-delete]
      (safe-delete (.getPath ^File file)))))

(defn delete-directory [directory-path]
  (do (delete-directory-contents directory-path)
      (safe-delete directory-path)))

(defn case-insensitive-compare [a b]
  (cond (and (nil? a) (nil? b)) 0
        (nil? a) 1
        (nil? b) -1
        :else (compare (str/upper-case a) (str/upper-case b))))

(defn set-ci
  ([] (sorted-set-by case-insensitive-compare))
  ([& keys] (apply sorted-set-by case-insensitive-compare keys)))

(defn map-ci
  ([] (sorted-map-by case-insensitive-compare))
  ([& keyvals] (apply sorted-map-by case-insensitive-compare keyvals)))

(def sort-distinct (comp sort distinct))

(defn first-char-uppercase? [s]
  (let [first-char (str (first s))]
    (re-find #"\p{Upper}" first-char)))

(defn first-char-numeric? [s] (numeric? (str (first s))))

(defn replace-several
  [str & replacements]
  (reduce (fn [s [a b]] (str/replace s a b)) str (partition 2 replacements)))

(defn mapmap
  "Apply kf and vf to a sequence, s, and produce a map of (kf %) to (vf %)."
  ([vf s] (mapmap identity vf s))
  ([kf vf s] (zipmap (map kf s) (map vf s))))

(defn deep-merge
     "Like merge, but merges maps recursively."
     [& maps]
     (if (every? map? maps)
       (apply merge-with deep-merge maps)
       (last maps)))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level."
     [f & maps]
     (apply
       (fn m [& maps]
         (if (every? map? maps)
           (apply merge-with m maps)
           (apply f maps)))
       maps))

(defn coerce-unformattable-types [args]
  (map
    (fn [x]
      (cond (instance? BigInt x) (biginteger x)
            (instance? Ratio x) (double x)
            :else x))
    args))

(defn format-plus [fmt & args] (apply format fmt (coerce-unformattable-types args)))

(defn avg-rd [avg i x] (float (+ avg (/ (- x avg) (inc i)))))

(defn assoc-in*
  "Associates value(s) in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  ([m [k & ks] v]
   (if ks
     (assoc m k (assoc-in (get m k) ks v))
     (assoc m k v)))
  ([m ks v & kvs]
   (let [ret (assoc-in m ks v)]
     (if kvs
       (if (next kvs)
         (recur ret (first kvs) (second kvs) (nnext kvs))
         (throw (IllegalArgumentException.
                 "assoc-in expects an even number of arguments after map, found odd number")))
       ret))))

;(defn nill-safe [f & args] (when (every? some? args) (apply f args)))

;(defn reduce-xf [f init]
;  (fn [rf]
;    (let [acc (volatile! init)]
;      (completing (fn [result input] (rf result (vswap! acc f input)))))))
;
;(defn reduce-chan-with-state [f init]
;  (let [c (async/chan 1 (reduce-xf f init))]
;    (async/go-loop [v (<! c)] (when v (println (str v)) (recur (<! c))))
;    (async/onto-chan c (range 100))))
