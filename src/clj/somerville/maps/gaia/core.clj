;; http://experilous.com/1/blog/post/procedural-planet-generation
;; https://github.com/Engelberg/ubergraph
(ns somerville.maps.gaia.core
  (:require
    [clojure.set :as s]
    [ubergraph.core :as uber]
    [somerville.geometry.commons :as gcommons]
    [somerville.geometry.triangle :as triangle]
    [somerville.geometry.point :as point]
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.line :as line]))

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

(def cube-corners
  (list
    (point/point -1 -1 -1)
    (point/point -1 -1  1)
    (point/point -1  1 -1)
    (point/point -1  1  1)
    (point/point  1 -1 -1)
    (point/point  1 -1  1)
    (point/point  1  1 -1)
    (point/point  1  1  1)))

(defn closest
  "Find the points in ps that are closest to a point p."
  [p ps]
  (let [distances (rest (sort-by first (map #(vector (point/distance p %) %) ps)))
        ref-dist (first (first distances))]
    (map #(apply line/line (sort (list p (second %)))) (take-while #(gcommons/close-to ref-dist (first %)) distances))))

(defn lines
  "Create lines from a set of points. Each point is connected to all those that are closest to it."
  [points]
  (distinct (sort (reduce concat (map #(closest % points) points)))))

(defn triangles
  "Create a set of triangles from a set of lines. If two lines share a point create the triangle from
  first point to shared to last point and thus back to first."
  [lines ignore-close]
  (filter #(not (nil? %))
    (for [l1 lines
          l2 lines]
      (when (not= l1 l2)
        (when (and
                (or ignore-close (gcommons/close-to (point/distance (:p1 l1) (:p2 l1)) (point/distance (:p1 l1) (:p2 l2))))
                (= (:p2 l1) (:p1 l2)))
          (triangle/triangle (:p1 l1) (:p2 l1) (:p2 l2)))))))

(defn icosahedron
  "Create a set of triangles that represent an icosahedron."
  [scale]
  (triangles (map #(line/scale % scale) (lines icosahedron-corners)) false))

(defn cube
  "Create a list of lines that represent a cube."
  [scale]
  (triangles (map #(line/scale % scale) (lines cube-corners)) true))

(defn subdivide
  "Subdivide each triangle in a list."
  [ts]
  (into '() (apply s/union (map #(into #{} (triangle/subdivide %)) ts))))

(defn polygonate
  [p ts]
  (let [affected (filter #(or (= (:p1 %) p (= (:p2 %) p)) (= (:p3 %) p)) ts)
        centers (map #(point/scale (point/add (:p1 %) (point/add (:p2 %) (:p3 %))) 1/3) affected)]
    (polygon/from-points centers)))

(defn triangle-points
  "Get all unique points from list of triangles."
  [ts]
  (apply s/union (map #(into #{} (list (:p1 %) (:p2 %) (:p3 %))) ts)))

(defn translate-surface
  "Translate triangle surface to polygon for each triangle point."
  [ts]
  (map #(polygonate % ts) (triangle-points ts)))

;;=================================================================================================================
;; Create a graph that holds all triangles and connects them to their neighbours.

(defn share-edge?
  "Check if two triangles share an edge. They do so if they share two points."
  [t1 t2]
  (= 4 (count (s/union #{(:p1 t1) (:p2 t1) (:p3 t1)} #{(:p1 t2) (:p2 t2) (:p3 t2)}))))

(defn neighbours
  "Get list of neighbors in ts for t as pairs [t1 t2]."
  [t ts]
  (into #{} (filter #(not (nil? %)) (map #(if (share-edge? t %) [t %] nil) (remove #{t} ts)))))

(defn create-edges
  "Create edges of triangles."
  [ts]
  (into '() (apply s/union (map #(neighbours % ts) ts))))

(defn create-graph
  [ts]
  (apply uber/add-edges (apply uber/add-nodes (uber/graph) ts) (create-edges ts)))

