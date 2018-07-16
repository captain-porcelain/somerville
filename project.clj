(defproject somerville "1.0.9-SNAPSHOT"
  :description "Implementation of some mathematical functions."
  :dependencies
  [[org.clojure/clojure "1.9.0"]
   [org.clojure/clojurescript "1.10.238"]
   [dali "0.7.4"]
   [quil "2.6.0"]
   [fs "1.3.3"]
   [ubergraph "0.5.0"]
   [com.taoensso/timbre "4.10.0"]]

  :plugins
  [[lein-cljsbuild "1.1.7"]
   [lein-marginalia "0.8.0"]]
  ;:main somerville.core
  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :repl-options
  {:timeout 120000}
  :hooks [leiningen.cljsbuild]

  :cljsbuild
  {:builds
   [{:source-paths ["src/cljs" "src/cljc"]
     :compiler
     {:output-to "resources/js/somerville.js"
      :optimizations :whitespace
      ;:optimizations :advanced
      :pretty-print true}}]})
