(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.visualization.voronoi :as voronoi]
            [sanakan.mathematics.visualization.fortune :as fortune]
            [sanakan.mathematics.visualization.fill :as fill]
            [sanakan.mathematics.visualization.quilt :as quilt]
            [sanakan.mathematics.visualization.l-system :as l-system]
            [quil.core :as quil])
  (:gen-class))

(defn -main [& args]
  ;(l-system/show))
  (fortune/show))

