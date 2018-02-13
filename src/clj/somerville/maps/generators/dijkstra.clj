(ns somerville.maps.generators.dijkstra
  (:require
    [somerville.commons :as commons]
    [somerville.maps.grid :as grid]))

(defn dijkstra-flooder
  "Set the distance of a node."
  [g c distance]
  (grid/update-cell g c #(assoc % :distance distance)))

(defn dijkstra
  "Create representation of grid as distances from origin point 0,0."
  [g]
  (let [g2 (grid/flood g dijkstra-flooder)]
    (for [y (range (:height g2))]
      (for [x (range (:width g2))]
        (:distance (grid/get-from g2 x y))))))

