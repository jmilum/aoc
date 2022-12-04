(ns aoc.core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.repl :refer [apropos dir doc find-doc pst source]]
   [clojure.edn :as edn]
   [clojure.java.math :as math]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 1
(def test1 ")())())")

(def floors {"(" 1 ")" -1})

(defn sum-rx [sum k v]
  (let [new-sum (+ sum v)]
    (if (< new-sum 0)
      (reduced (inc k))
      new-sum)))

#_(->> (slurp "resources/1.txt")
       (map str)
       (mapv floors)
       (reduce-kv sum-rx 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 2
(defn load-data [file]
  (-> (slurp file)
      (str/split-lines)
      (->> (mapv #(str/split % #"x"))
           (map #(mapv parse-long %)))))

(defn area [acc v]
  (let [[x y z] v
        a1   (* x y)
        a2   (* x z)
        a3   (* y z)
        amin (min a1 a2 a3)]
    (+ acc (* 2 a1) (* 2 a2) (* 2 a3) amin)))

#_(->> (load-data "resources/2.txt")
       (reduce area 0))

(defn feet [acc v]
  (let [perim (* 2 (apply + (butlast (sort v))))
        bow   (apply * v)]
    (+ acc perim bow)))

#_(->> (load-data "resources/2.txt")
       (reduce feet 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 3
(defn load-data [file]
  (-> (slurp file)
      (str/split #"")))

(defn update-coords [[x y] dir]
  (condp = dir
    "^" [x (inc y)]
    "v" [x (dec y)]
    ">" [(inc x) y]
    "<" [(dec x) y]))

;; part1
#_(->> (load-data "resources/3.txt")
       (reductions update-coords [0 0])
       (distinct)
       (count))

;; part2
#_(let [data  (load-data "resources/3.txt")
        santa (reductions update-coords [0 0] (take-nth 2 data))
        robo  (reductions update-coords [0 0] (take-nth 2 (rest data)))]
    (-> (into santa robo)
        distinct
        count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 4
#_(defn md5
    [^String s]
    (->> s
         .getBytes
         (.digest (MessageDigest/getInstance "MD5"))
         (BigInteger. 1)
         (format "%032x")))

(def secret "yzbqklnj")

#_(for [n (range 10000)
        :let [h (md5 (str secret n))]
        :while (not (str/starts-with? h "00000"))]
    n)

#_(loop [n 1]
    (let [h (md5 (str secret n))]
      (if (or (> n 10000000)
              (str/starts-with? h "000000"))
        n
        (recur (inc n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 5
#_(->> (slurp "resources/5.txt")
       (str/split-lines)
       (filter #(re-matches #".*[aeiou].*[aeiou].*[aeiou].*" %))
       (filter #(re-matches #".*(.)\1.*" %))
       (remove #(re-matches #".*ab.*|.*cd.*|.*pq.*|.*xy.*" %))
       (count))

#_(->> (slurp "resources/5.txt")
       (str/split-lines)
       (filter #(re-matches #".*?([a-z]{2}).*?(\1).*" %))
       (filter #(re-matches #".*?([a-z]).\1.*" %))
       (count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 6

(defn parse [s]
  (let [action {"toggle"   :toggle
                "turn on"  :on
                "turn off" :off}
        m      (re-matches #"(\w+\s?\w*) (\d+)\,(\d+) through (\d+)\,(\d+)" s)]
    {:action (action (nth m 1))
     :x1     (parse-long (nth m 2))
     :y1     (parse-long (nth m 3))
     :x2     (parse-long (nth m 4))
     :y2     (parse-long (nth m 5))}))

(defn load-data []
  (->> (slurp "resources/6.txt")
       (str/split-lines)
       (map parse)))

(defn eval-action [acc m]
  (let [{:keys [x1 y1 x2 y2 action]} m
        coords  (for [x (range x1 (inc x2))
                      y (range y1 (inc y2))]
                  [x y])
        actions {:toggle (fn [a b] (update a b #(if ((fnil zero? 0) %) 1 0)))
                 :on     #(assoc %1 %2 1)
                 :off    #(assoc %1 %2 0)}]
    (reduce (actions action) acc coords)))

#_(eval-action {} (take 1 (load-data)))

#_(->> (load-data)
       (reduce eval-action {})
       (vals)
       (reduce +))


(defn eval-action [acc m]
  (let [{:keys [x1 y1 x2 y2 action]} m
        coords  (for [x (range x1 (inc x2))
                      y (range y1 (inc y2))]
                  [x y])
        actions {:toggle (fn [a b] (update a b #((fnil + 0) % 2)))
                 :on     (fn [a b] (update a b (fnil inc 0)))
                 :off    (fn [a b] (update a b #(if ((fnil pos? -1) %) (dec %) 0)))}]
    (reduce (actions action) acc coords)))

#_(->> (load-data)
       (reduce eval-action {})
       (vals)
       (reduce +))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 6

(def ops
  {:ident  identity
   :and    (fn [[x y]] (bit-and 16rFFFF x y))
   :or     (fn [[x y]] (bit-and 16rFFFF (bit-or x y)))
   :not    (fn [[x]] (bit-and 16rFFFF (bit-not x)))
   :rshift (fn [[x n]] (bit-and 16rFFFF (unsigned-bit-shift-right x n)))
   :lshift (fn [[x n]] (bit-and 16rFFFF (bit-shift-left x n)))})

(defn parse-val [s] (or (parse-long s) (keyword s)))

(defn parse [s]
  (condp re-find s
    #"NOT (\w+) -> (\w+)" :>> #(hash-map
                                (keyword (nth % 2))
                                {:op :not
                                 :in [(keyword (nth % 1))]})
    #"(\w+) AND (\w+) -> (\w+)" :>> #(hash-map
                                      (keyword (nth % 3))
                                      {:op :and
                                       :in [(parse-val (nth % 1)) (parse-val (nth % 2))]})
    #"(\w+) OR (\w+) -> (\w+)" :>> #(hash-map
                                     (keyword (nth % 3))
                                     {:op :or
                                      :in [(parse-val (nth % 1)) (parse-val (nth % 2))]})
    #"(\w+) LSHIFT (\w+) -> (\w+)" :>> #(hash-map
                                         (keyword (nth % 3))
                                         {:op :lshift
                                          :in [(parse-val (nth % 1)) (parse-val (nth % 2))]})
    #"(\w+) RSHIFT (\w+) -> (\w+)" :>> #(hash-map
                                         (keyword (nth % 3))
                                         {:op :rshift
                                          :in [(parse-val (nth % 1)) (parse-val (nth % 2))]})
    #"(\w+) -> (\w+)" :>> #(hash-map
                            (keyword (nth % 2))
                            {:op :ident
                             :in (parse-val (nth % 1))})))

(defn load-data []
  (->> (slurp "resources/7.txt")
       (str/split-lines)
       (map parse)
       (into {})))

#_(->> (load-data))

(def test1
  ["123 -> x"
   "456 -> y"
   "x AND y -> d"
   "x OR y -> e"
   "x LSHIFT 2 -> f"
   "y RSHIFT 2 -> g"
   "NOT x -> h"
   "NOT y -> i"])

;d: 72
;e: 507
;f: 492
;g: 114
;h: 65412
;i: 65079
;x: 123
;y: 456

(defn update-node [m k]
  (let []))

(let [net (->> (map parse test1) (into {}))]
  (:d net))

(loop [m (->> (map parse test1) (into {}))
       k :x]
  (let [node (get m k)
        in   (:in node)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; day 3 2021

(def data
  [[0 0 1 0 0]
   [1 1 1 1 0]
   [1 0 1 1 0]
   [1 0 1 1 1]
   [1 0 1 0 1]
   [0 1 1 1 1]
   [0 0 1 1 1]
   [1 1 1 0 0]
   [1 0 0 0 0]
   [1 1 0 0 1]
   [0 0 0 1 0]
   [0 1 0 1 0]])

(defn rx [acc s]
  (->> (str/split s #"")
       (map parse-long)
       (map + acc)))

(defn bit-list->dec [xs] (edn/read-string (str "2r" (str/join xs))))

(defn load-data []
  (->> (slurp "resources/2021-3.txt")
       (str/split-lines)))

#_(defn aoc-3-1 []
    (let [data    (load-data)
          half    (/ (count data) 2)
          init    (repeat (count (first data)) 0)
          totals  (reduce rx init data)
          gamma   (map #(if (> % half) 1 0) totals)
          epsilon (map {0 1 1 0} gamma)]
      (* (bit-list->dec gamma) (bit-list->dec epsilon))))

#_(aoc-3-1)

(defn load-data []
  (->> (slurp "resources/2021-3.txt")
       (str/split-lines)
       (map #(str/split % #""))
       (map #(map parse-long %))))


;(loop [data (load-data)
;       bit 0]
;  (let [half (/ (count data) 2)
;        init (repeat (count (first data)) 0)
;        totals (reduce rx init data)
;
;        gamma (map #(if (>= % half) 1 0) totals)
;        epsilon (map {0 1 1 0} gamma)]
;   (* (bit-list->dec gamma) (bit-list->dec epsilon))))

#_(loop [data (load-data)
         bit  0]
    (let [xs        (apply map vector data)
          half      (/ (count (first xs)) 2)
          total     (reduce + (nth xs bit))
          keep-val  (if (>= total half) 1 0)
          keep-list (filter #(= keep-val (nth % bit)) data)]
      (if (> (count keep-list) 1)
        (recur keep-list (inc bit))
        (first keep-list))))
; (0 0 0 1 1 1 1 0 0 1 1 0)

#_(loop [data (load-data)
         bit  0]
    (let [xs        (apply map vector data)
          half      (/ (count (first xs)) 2)
          total     (reduce + (nth xs bit))
          keep-val  (if (< total half) 1 0)
          keep-list (filter #(= keep-val (nth % bit)) data)]
      (if (> (count keep-list) 1)
        (recur keep-list (inc bit))
        (first keep-list))))
; (1 0 1 0 1 1 1 0 0 0 0 0)

#_(* (bit-list->dec [0 0 0 1 1 1 1 0 0 1 1 0]) (bit-list->dec [1 0 1 0 1 1 1 0 0 0 0 0]))

;;; 2021 day 4
(def data
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
  22 13 17 11 0
  8  2 23  4 24
  21  9 14 16  7
  6 10  3 18  5
  1 12 20 15 19

  3 15  0  2 22
  9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
  2  0 12  3  7")

(defn create-board-set [acc xs]
  (let [horiz (mapv #(into #{} %) xs)
        vert  (mapv #(into #{} %) (apply map vector xs))]
    (->> horiz
         (into vert)
         (conj acc))))

(defn parse-data [s]
  (let [data    (str/split s #"\s+")
        numbers (mapv parse-long (str/split (first data) #","))
        boards  (->> (rest data)
                     (map parse-long)
                     (partition 5)
                     (partition 5)
                     (reduce create-board-set []))]
    [numbers boards]))

(defn winner? [number-set board-set]
  (boolean (some (partial set/superset? number-set) board-set)))

(defn winner-rx [boards acc number]
  (let [number-set (conj acc number)
        winner     (filter (partial winner? number-set) boards)]
    (if (seq winner)
      (reduced {:number-set number-set
                :board      (first winner)
                :number     number})
      number-set)))

(defn get-score [numbers boards]
  (let [init        (into #{} (take 4 numbers))
        number-list (nthrest numbers 4)
        winner      (reduce (partial winner-rx boards) init number-list)
        all-numbers (apply set/union (:board winner))
        unmatched   (set/difference all-numbers (:number-set winner))]
    (* (:number winner) (apply + unmatched))))

(defn loser-rx [acc number]
  (let [{:keys [number-set boards winners]} acc
        number-set  (conj number-set number)
        winner      (filter (partial winner? number-set) boards)
        boards-left (remove (partial winner? number-set) boards)]
    (if (seq winner)
      {:number-set number-set
       :boards     boards-left
       :number     number
       :winners    (conj winners winner)}
      (if (seq boards-left)
        (assoc acc
               :number-set number-set
               :boards boards)
        (reduced (assoc acc :boards boards))))))

(defn get-loser [numbers boards]
  (let [init        (into #{} (take 4 numbers))
        number-list (nthrest numbers 4)
        winner      (reduce loser-rx {:number nil :number-set init :boards boards :winners []} number-list)
        all-numbers (apply set/union (first (last (:winners winner))))
        unmatched   (set/difference all-numbers (:number-set winner))]
    (* (:number winner) (apply + unmatched))))

(let [data (slurp "resources/2021-4.txt")
      [numbers boards] (parse-data data)]
  (get-loser numbers boards))

;; day 5

(defn line-segment [[x1 y1 x2 y2]]
  (let [xdiff   (- x2 x1)
        ydiff   (- y2 y1)
        maxdiff (max (math/abs ^long xdiff) (math/abs ^long ydiff))
        dx      (/ xdiff maxdiff)
        dy      (/ ydiff maxdiff)]
    (for [i (range (inc maxdiff))]
      [(math/round ^float (+ x1 (* i dx))) (math/round ^float (+ y1 (* i dy)))])))

#_(->> (slurp "resources/2021-5.txt")
       (str/split-lines)
       (map #(re-matches #"(\d+)\,(\d+) \-\> (\d+)\,(\d+)" %))
       (mapcat rest)
       (map parse-long)
       (partition-all 4)
       ;(filter (fn [[x1 y1 x2 y2]] (or (= x1 x2) (= y1 y2))))
       (mapcat line-segment)
       (frequencies)
       (filter #(>= (val %) 2))
       (count))

;;; day 9

(def data-raw (->> "resources/2021-9.txt" slurp str/split-lines))
(def cols (->> data-raw first count))
(def data (->> data-raw (mapcat #(str/split % #"")) (mapv parse-long)))

(defn left-edge? [index] (= 0 (mod index cols)))
(defn right-edge? [index] (= (dec cols) (mod index cols)))
(defn top-edge? [index] (< index cols))
(defn bottom-edge? [index] (>= index (- (count data) cols)))

(defn up [index]
  (when-not (top-edge? index)
    (let [up-index (- index cols)]
      [up-index (get data up-index)])))

(defn down [index]
  (when-not (bottom-edge? index)
    (let [down-index (+ index cols)]
      [down-index (get data down-index)])))

(defn left [index]
  (when-not (left-edge? index)
    (let [left-index (dec index)]
      [left-index (get data left-index)])))

(defn right [index]
  (when-not (right-edge? index)
    (let [right-index (inc index)]
      [right-index (get data right-index)])))

(defn expand-point [index]
  (keep #(% index) [up down left right]))

(defn low-point [index value]
  (when (->> (expand-point index)
             (every? #(< value (second %))))
    [index value]))

;; part 1
#_(->> data
       (keep-indexed low-point)
       (map second)
       (map inc)
       (reduce +))

(defn basin-points [seen [index _]]
  (->> (expand-point index)
       (remove #(= 9 (second %)))
       (remove seen)))

(defn basin-rx [basins point]
  (loop [points (basin-points #{} point)
         seen   #{point}]
    (if (seq points)
      (let [point            (first points)
            remaining-points (rest points)
            new-points       (basin-points seen point)]
        (recur (into remaining-points new-points)
               (into seen #{point})))
      (conj basins seen))))

;; part 2
#_(let [basins (keep-indexed low-point data)]
    (->> basins
         (reduce basin-rx [])
         (map count)
         (sort >)
         (take 3)
         (apply *)))

;; day 10


#_(def data (->> "resources/10.txt"
                 slurp
                 str/split-lines
                 (map #(str/split % #""))))

(def open #{"(" "{" "[" "<"})
(def close #{")" "}" "]" ">"})

(def pairs {")" "("
            "]" "["
            "}" "{"
            ">" "<"})

(def points {")" 3 "]" 57 "}" 1197 ">" 25137})

(defn init-state []
  {:stack []
   :error :none})

(defn update-state [state item]
  (condp contains? item
    open (update state :stack conj item)
    close (if (= (get pairs item) (peek (:stack state)))
            (update state :stack pop)
            (-> state
                (update :stack conj item)
                (assoc :error :corrupt)
                (reduced)))))

#_(->> data
       (map #(reduce update-state (init-state) %))
       (filter #(= :corrupt (:error %)))
       (map #(peek (:stack %)))
       (map points)
       (apply +))

(def pairs-inv (set/map-invert pairs))
(def points-incomplete {")" 1 "]" 2 "}" 3 ">" 4})

(defn middle-value [v]
  (when-not (empty? v)
    (nth v (quot (count v) 2))))

#_(->> data
       (map #(reduce update-state (init-state) %))
       (remove #(= :corrupt (:error %)))
       (map :stack)
       (map #(map pairs-inv %))
       (map reverse)
       (map #(map points-incomplete %))
       (map #(reduce (fn [acc n] (+ n (* acc 5))) 0 %))
       (sort)
       (middle-value))

;; day 11

#_(def data-raw (->> "resources/11.txt" slurp str/split-lines))
(def cols (->> data-raw first count))
(def data (->> data-raw (mapcat #(str/split % #"")) (mapv parse-long)))

(defn left-edge? [index] (= 0 (mod index cols)))
(defn right-edge? [index] (= (dec cols) (mod index cols)))
(defn top-edge? [index] (< index cols))
(defn bottom-edge? [index] (>= index (- (count data) cols)))

(defn up [index] (when-not (top-edge? index) (- index cols)))
(defn up-left [index] (when-not (or (top-edge? index) (left-edge? index)) (- (dec index) cols)))
(defn up-right [index] (when-not (or (top-edge? index) (right-edge? index)) (- (inc index) cols)))
(defn down [index] (when-not (bottom-edge? index) (+ index cols)))
(defn down-left [index] (when-not (or (bottom-edge? index) (left-edge? index)) (+ (dec index) cols)))
(defn down-right [index] (when-not (or (bottom-edge? index) (right-edge? index)) (+ (inc index) cols)))
(defn left [index] (when-not (left-edge? index) (dec index)))
(defn right [index] (when-not (right-edge? index) (inc index)))

(defn expand-point [index] (keep #(% index) [up down left right up-left up-right down-left down-right]))

(defn update-state [state]
  (let [{:keys [grid flashes step]} state]
    (loop [grid (mapv inc grid)
           seen #{}
           step (inc step)]
      (let [flash (set/difference (into #{} (keep-indexed (fn [i n] (when (> n 9) i)) grid)) seen)]
        (if (empty? flash)
          {:grid    (reduce #(if (> %2 9) (conj %1 0) (conj %1 %2)) [] grid)
           :flashes (+ flashes (count seen))
           :step    step}
          (recur (->> (mapcat expand-point flash)
                      (reduce #(update %1 %2 inc) grid))
                 (into seen flash)
                 step))))))


#_(->> {:grid data :flashes 0 :step 0}
       (iterate update-state)
       (take 101)
       (last)
       :flashes)

#_(loop [state {:grid data :flashes 0 :step 0}]
    (let [new-state (update-state state)]
      (if (every? zero? (:grid new-state))
        (:step new-state)
        (recur new-state))))

;; day 12

(def data-str "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")

(defn load-data [s]
  (->> (str/split-lines s)
       (map #(str/split % #"-"))
       (mapcat (fn [[a b]] [{a #{b} b #{a}}]))
       (apply merge-with set/union)))

#_(load-data data-str)

(defn lower-case? [node] (= node (str/lower-case node)))
