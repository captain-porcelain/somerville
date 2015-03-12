(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.visualization.voronoi :as voronoi]
            [sanakan.mathematics.visualization.fill :as fill]
            [quil.core :as quil])
  (:gen-class))

(defn -main [& args]
  (fill/show))

