(defproject somerville "1.0.9-SNAPSHOT"
  :description "Implementation of some mathematical functions."
  :dependencies
  [[org.clojure/clojure "1.10.1"]
   [org.clojure/clojurescript "1.10.597"]
   [dali "0.7.4"]
   [quil "2.6.0"]
   [fs "1.3.3"]
   [ubergraph "0.5.0"]
   [com.taoensso/timbre "4.10.0"]]

  :plugins
  [[lein-cljsbuild "1.1.7"]
   [lein-marginalia "0.9.1"]]
  ;:main somerville.core
  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :repl-options
  {:timeout 120000}
  :hooks [leiningen.cljsbuild]

  :cljsbuild
  {:builds
   [{:source-paths ["src/cljs" "src/cljc"]
     :compiler
     {:output-dir "resources/js/"
      :output-to "resources/js/somerville.js"
      ;; To add source maps add this:
      :source-map "resources/js/somerville.js.map"
      :optimizations :whitespace}}]}
      ;:optimizations :advanced}}]}
  )
