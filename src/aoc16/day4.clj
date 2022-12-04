(ns advent.day4
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clojure.math.numeric-tower :as math]
    [advent.util :as util :refer [->map]]))

(defn name-comp [a b]
  (let [k1 (str (first a))
        k2 (str (first b))
        v1 (second a)
        v2 (second b)]
    (if (= v1 v2)
      (compare k1 k2)
      (- v2 v1))))

(defn name-crc [s]
  (->> (frequencies s)
       (sort name-comp)
       (map first)
       (take 5)
       (apply str)))

(defn sum-id [acc m]
  (let [{:keys [name-crc crc sector]} m]
    (if (= crc name-crc) (+ acc sector) acc)))

(defn decode [k s]
  (->> (str/upper-case s)
       (map int)
       (map #(+ % (mod k 26)))
       (map #(if (> % 90) (+ 64 (- % 90)) %))
       (map char)))


(defn data []
  (->> (slurp "resources/day4.txt")
     (str/split-lines)
     (map #(str/split % #"-"))
     (map #(let [name (butlast %)
                 name-str (str/join name)
                 sector-crc (str/split (last %) #"\[|\]")]
             (hash-map :name name
                       :name-crc (name-crc name-str)
                       :sector (util/str->int (first sector-crc))
                       :crc (second sector-crc))))
     (filter #(= (:name-crc %) (:crc %)))
     (map #(hash-map :name (apply str (decode (:sector %) (:name %)))
                     :sector (:sector %)))))



;(defn name-comp [a b]
;  (let [k1 (str (first a))
;        k2 (str (first b))
;        v1 (second a)
;        v2 (second b)]
;    (if (= v1 v2)
;      (compare k1 k2)
;      (- v2 v1))))
;
;(defn name-crc [s]
;  (->> (frequencies s)
;       (sort name-comp)
;       (map first)
;       (take 5)
;       (apply str)))
;
;(defn sum-id [acc m]
;  (let [{:keys [name-crc crc sector]} m]
;    (if (= crc name-crc) (+ acc sector) acc)))
;
;(->> (slurp "resources/day4.txt")
;     (str/split-lines)
;     (map #(str/split % #"-"))
;     (map #(let [name (butlast %)
;                 name-str (str/join name)
;                 sector-crc (str/split (last %) #"\[|\]")]
;             (hash-map :name name
;                       :name-crc (name-crc name-str)
;                       :sector (util/str->int (first sector-crc))
;                       :crc (second sector-crc))))
;     (reduce sum-id 0))
