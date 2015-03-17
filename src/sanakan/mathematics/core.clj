(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.visualization.voronoi :as voronoi]
            [sanakan.mathematics.visualization.fill :as fill]
            [sanakan.mathematics.visualization.quilt :as quilt]
            [quil.core :as quil])
  (:gen-class))

(defn -main [& args]
  (quilt/show))

