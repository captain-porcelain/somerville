(defproject mathematics "1.0.0-SNAPSHOT"
  :description "Implementation of some mathematical functions."
  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [quil "2.2.2"]
   [org.clojure/math.numeric-tower "0.0.4"]
   ;; tools
   [midje "1.6.3"]
   [org.clojure/core.typed "0.2.71"]
   [bultitude "0.2.6"]]
  :plugins
  [[lein-typed "0.3.5"] ; do static type checking
   [lein-ancient "0.5.5" :exclusions [org.clojure/clojure]] ; find outdated dependencies
   [lein-marginalia "0.8.0"] ; create documentation
   [lein-midje "3.1.3"]]
  :core.typed
  {:check
   [sanakan.mathematics.geometry.point]}
  :main sanakan.mathematics.core)
