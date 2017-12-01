(ns aoc.day1
  (:require
   [clojure.main]
   [clojure.set :as set]
   [clojure.data.csv :as csv]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]
   [clojure.math.combinatorics :as combo]
   [clojure.algo.generic.functor :as f]
   [aoc.util :as util]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
