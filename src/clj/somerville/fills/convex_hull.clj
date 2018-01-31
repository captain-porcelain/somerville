;; Implementation of Quick Hull (https://en.wikipedia.org/wiki/Quickhull)
(ns somerville.fills.convex-hull
  (:require
    [clojure.set :as s]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.polygon :as poly]))

(defn furthest-point
  [point points]
  (second (last (sort-by first (map #(vector (p/distance point %) %) points)))))

(defn first-last-by-x
  "Get line between the points with minimun x and maximum x."
  [points]
  (let [sorted (sort-by :x points)]
    (l/line (first sorted) (last sorted))))

(defn above?
  "Check if a point is above a line."
  [point line]
  (> (:y point) (l/solve-line-at line (:x point))))

(defn partition-points
  "Partition points by given line."
  [points line]
  (map #(map second %) (partition-by first (sort-by first (map #(vector (above? % line) %) points)))))
