(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.visualization.voronoi :as voronoi]
            [quil.core :as quil])
  (:gen-class))

(defn -main [& args]
  (voronoi/show))

