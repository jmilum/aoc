(ns user
  (:require
    [clojure.main]
    [clojure.pprint :refer [pprint pp]]
    [clojure.repl :refer [apropos dir doc find-doc pst source]]
    [clojure.set :as set]
    [clojure.data.csv :as csv]
    [clojure.string :as str]
    [clojure.math.numeric-tower :as math]
    [clojure.math.combinatorics :as combo]
    [clojure.tools.namespace.repl :refer [refresh refresh-all]]
    [clojure.algo.generic.functor :as f]
    [criterium.core :refer [bench quick-bench]]
    [aoc.util :as util]
    [aoc.core :as core]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


