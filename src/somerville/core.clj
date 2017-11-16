(ns somerville.core
  (:require [somerville.visualization.voronoi :as voronoi]
            [somerville.visualization.fortune :as fortune]
            [somerville.visualization.fill :as fill]
            [somerville.visualization.quilt :as quilt]
            [somerville.visualization.l-system :as l-system]
            [quil.core :as quil])
  (:gen-class))

(defn -main [& args]
  ;(l-system/show))
  (fortune/show))

