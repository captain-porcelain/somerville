(defproject mathematics "1.0.0-SNAPSHOT"
  :description "Implementation of some mathematical functions."
  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [quil "2.3.0"]
   [org.clojure/math.numeric-tower "0.0.4"]
   ;; tools
   [midje "1.8.3"]]
  :plugins
  [[lein-marginalia "0.8.0"] ; create documentation
   [lein-midje "3.2"]]
  :main sanakan.mathematics.core)
