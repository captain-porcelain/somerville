(defproject somerville "1.0.5-SNAPSHOT"
  :description "Implementation of some mathematical functions."
  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [quil "2.6.0"]
   [fs "1.3.3"]
   [com.taoensso/timbre "4.10.0"]]
  :plugins
  [[lein-marginalia "0.8.0"]]
  :main somerville.core
  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :repl-options
  {:timeout 120000})
