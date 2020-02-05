(defproject somerville "1.0.9-SNAPSHOT"
  :description "Implementation of some mathematical functions."
  :dependencies
  [[org.clojure/clojure "1.10.1"]
   [org.clojure/clojurescript "1.10.597"]
   [org.clojure/core.async "0.7.559"]
   [com.taoensso/timbre "4.10.0"]
   [quil "3.1.0"]
   [reagent "0.9.1"]
   [dali "0.7.5"]

   ;; web interface for local testing
   [compojure "1.6.1"]
   [ring/ring-core "1.8.0"]
   [ring/ring-servlet "1.8.0"]
   [ring/ring-jetty-adapter "1.8.0"]
   [ring/ring-codec "1.1.2"]
   [http-kit "2.3.0"]]

  :plugins
  [[lein-cljsbuild "1.1.7"]
   [lein-marginalia "0.9.1"]]
  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :test-paths ["src/clj" "src/cljc" "src/cljs"]
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
