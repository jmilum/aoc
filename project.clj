(defproject aoc "0.1.0"
  :repositories
  [["clojars" {:url "https://clojars.org/repo/"}]
   ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
  :dependencies [[org.clojure/clojure "1.9.0-RC2"]
                 [org.clojure/algo.generic "0.1.2"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [criterium "0.4.4"]
                 [org.clojure/tools.namespace "0.3.0-alpha4"]]
  :profiles {:uberjar {:aot :all}}
  :source-paths ["src"]
  :resource-paths ["resources"]
  :repl-options {:init-ns user}
  :target-path "target/%s")
