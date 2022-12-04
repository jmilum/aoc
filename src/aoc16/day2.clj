(ns advent.day2
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clojure.math.numeric-tower :as math]
    [advent.util :as util :refer [->map]]))


(def start {:number :5 :code []})

(def data (->> (slurp "resources/day2.edn")
               (edn/read-string)
               (map #(str/split % #""))
               (map #(map keyword %))))

(def moves
  {:1 {:U :1 :R :1 :D :3 :L :1}
   :2 {:U :2 :R :3 :D :6 :L :2}
   :3 {:U :1 :R :4 :D :7 :L :2}
   :4 {:U :4 :R :4 :D :8 :L :3}
   :5 {:U :5 :R :6 :D :5 :L :5}
   :6 {:U :2 :R :7 :D :A :L :5}
   :7 {:U :3 :R :8 :D :B :L :6}
   :8 {:U :4 :R :9 :D :C :L :7}
   :9 {:U :9 :R :9 :D :9 :L :8}
   :A {:U :6 :R :B :D :A :L :A}
   :B {:U :7 :R :C :D :D :L :A}
   :C {:U :8 :R :C :D :C :L :B}
   :D {:U :B :R :D :D :D :L :D}})

(defn move [state direction] (assoc state :number (get-in moves [(:number state) direction])))

(defn digit [state leg]
  (let [number (:number (reduce move state leg))]
    (-> state
        (assoc :number number)
        (update :code conj number))))

(reduce digit start data)


;(def start {:number :5 :code []})
;
;(def data (->> (slurp "resources/day2.edn")
;               (edn/read-string)
;               (map #(str/split % #""))
;               (map #(map keyword %))))
;
;(def moves
;  {:1 {:U :1 :R :2 :D :4 :L :1}
;   :2 {:U :2 :R :3 :D :5 :L :1}
;   :3 {:U :3 :R :3 :D :6 :L :2}
;   :4 {:U :1 :R :5 :D :7 :L :4}
;   :5 {:U :2 :R :6 :D :8 :L :4}
;   :6 {:U :3 :R :6 :D :9 :L :5}
;   :7 {:U :4 :R :8 :D :7 :L :7}
;   :8 {:U :5 :R :9 :D :8 :L :7}
;   :9 {:U :6 :R :9 :D :9 :L :8}})
;
;(defn move [state direction] (assoc state :number (get-in moves [(:number state) direction])))
;
;(defn digit [state leg]
;  (let [number (:number (reduce move state leg))]
;    (-> state
;      (assoc :number number)
;      (update :code conj number))))
;
;(reduce digit start data)
