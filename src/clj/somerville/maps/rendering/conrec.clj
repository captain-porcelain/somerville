;; See http://paulbourke.net/papers/conrec/
(ns somerville.maps.rendering.conrec
  (:require
    [somerville.commons :as commons]
    [somerville.maps.grid :as grid]
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.point :as p]
    [taoensso.timbre :as log]))

(defn grid-points
  "Create list of all points in grid."
  [g]
  (let [size (dec (:width g))]
    (for [y (range size)
          x (range size)]
      [x y])))

(defn triangles
  "For a point the the 3 neighboring points with increasing x and y and create 4 triangles.
  The center point receives the average height value."
  [g [x y]]
  (let [z1 (get (grid/get-from g x y) :height 0)
        z2 (get (grid/get-from g (inc x) y) :height 0)
        z3 (get (grid/get-from g (inc x) (inc y)) :height 0)
        z4 (get (grid/get-from g x (inc y)) :height 0)
        z0 (/ (+ z1 z2 z3 z4) 4)
        p0 (p/point (+ x 0.5) (+ y 0.5) z0)
        p1 (p/point x y z1)
        p2 (p/point (inc x) y z2)
        p3 (p/point (inc x) (inc y) z3)
        p4 (p/point x (inc y) z4)
        t1 (polygon/from-points (list p1 p0 p2))
        t2 (polygon/from-points (list p2 p0 p3))
        t3 (polygon/from-points (list p3 p0 p4))
        t4 (polygon/from-points (list p4 p0 p1))]
    [t1 t2 t3 t4]))

(defn triangulation
  "Triangulate grid by creating 4 triangles for each point and its three neighbors.
  Must use loop instead of map and reduce concat since concat will blow the stack."
  [g]
  (loop [p2s (grid-points g)
         p3s (list)]
    (if (= 0 (count p2s))
      p3s
      (recur (rest p2s) (concat p3s (triangles g (first p2s)))))))
