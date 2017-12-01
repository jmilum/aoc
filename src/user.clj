(ns user
  (:require
    [clojure.main]
    [clojure.pprint :refer [pprint pp]]
    [clojure.repl :refer [apropos dir doc find-doc pst source]]
    [clojure.set :as set]
    [clojure.data.csv :as csv]
    [clojure.string :as str]
    [clojure.math.numeric-tower :as math]
    [clojure.tools.namespace.repl :refer [refresh refresh-all]]
    [clojure.algo.generic.functor :as f]
    [criterium.core :refer [bench quick-bench]]
    [aoc.util :as util]
    [aoc.day1 :as day1]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


