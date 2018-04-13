;; http://experilous.com/1/blog/post/procedural-planet-generation
;; https://github.com/Engelberg/ubergraph
(ns somerville.maps.gaia.core
  (:require
    [clojure.set :as s]
    [somerville.geometry.commons :as gcommons]
    [somerville.geometry.point :as point]
    [somerville.geometry.line :as line]))

(defrecord Node [data neighbors])

(defn graph
  ""
  []
  )

(def icosahedron-corners
  (list
    (point/point -0.26286500 0.0000000 0.42532500)
    (point/point 0.26286500 0.0000000 0.42532500)
    (point/point -0.26286500 0.0000000 -0.42532500)
    (point/point 0.26286500 0.0000000 -0.42532500)
    (point/point 0.0000000 0.42532500 0.26286500)
    (point/point 0.0000000 0.42532500 -0.26286500)
    (point/point 0.0000000 -0.42532500 0.26286500)
    (point/point 0.0000000 -0.42532500 -0.26286500)
    (point/point 0.42532500 0.26286500 0.0000000)
    (point/point -0.42532500 0.26286500 0.0000000)
    (point/point 0.42532500 -0.26286500 0.0000000)
    (point/point -0.42532500 -0.26286500 0.0000000)))

(defn closest
  "Find the points in ps that are closest to a point p."
  [p ps]
  (let [distances (rest (sort-by first (map #(vector (point/distance p %) %) ps)))
        ref-dist (first (first distances))]
    (map #(apply line/line (sort (list p (second %)))) (take-while #(gcommons/close-to ref-dist (first %)) distances))))

(defn create-net
  [points]
  (distinct (sort (reduce concat (map #(closest % points) points)))))

(defn icosahedron
  "Create a list of lines that represent an icosahedron."
  []
  (create-net icosahedron-corners))

(defn subdivide
  "Take a list of lines half each and create lines for each of the resulting points to its closest neighbors."
  [lines]
  ;; TODO don't use midpoint but slerp instead
  (let [midpoints (map #(point/midpoint (:p1 %) (:p2 %)) lines)
        points (s/union (map #(hash-set (:p1 %) (:p2 %)) lines))]
    (s/union (into #{} midpoints) points)))

