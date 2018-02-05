;; Implementation of Concave Hull (http://www.geosensor.net/papers/duckham08.PR.pdf)
(ns somerville.fills.concave-hull
  (:require
    [clojure.set :as s]
    [somerville.geometry.point :as p]
    [somerville.geometry.polygon :as poly]))

(defn k-nearest-neighbors
  "Find the k nearest neighbors of point in points. Assumes that point is not in points."
  [k point points]
  (map second (take k (sort-by first (map #(vector (p/distance point %) %) points)))))

(defn next-point
  "Select the next point from points from k nearest neighbors by max angle to line from point to 0,0."
  [k point old-point points]
  (second (last
    (sort-by first
      (map #(vector (p/angle-pos point old-point %) %)
           (k-nearest-neighbors k point points))))))

(defn hull-points
  "Find points of concave hull for set of points."
  [points k]
  (let [fp (first (sort-by :y points))]
    (loop [point fp
           old-point (p/point (- (:x fp) 1) (:y fp))
           hull (vector point)
           remaining (remove #{point} points)
           i 0]
      (let [new-point (next-point k point old-point remaining)
            new-remaining (remove #{new-point} remaining)]
        (if (= fp new-point)
          (conj hull new-point)
          (recur new-point point (conj hull new-point) (if (= 3 i) (conj new-remaining fp) new-remaining) (inc i)))))))

(defn hull
  "Create concave hull for set of points."
  [points]
  (poly/from-points (butlast (hull-points points 3))))
