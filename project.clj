(defproject interpreter "0.1.0-SNAPSHOT"
  :description "Simple LISP interpreter with dynamic scoping"
  :license {:name "GPLv3"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot interpreter.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
