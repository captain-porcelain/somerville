(ns somerville.geometry.delaunay
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p]))

(defn max-val
  "Get max k value of given points."
  [points k]
  (apply max (map k points)))

(defn bounding-triangle
  "Find a triangle that contains all points."
  [points]
  (list [0 0] [0 (* 2 (max-val points :y))] [(* 2 (max-val points :x)) 0]))
