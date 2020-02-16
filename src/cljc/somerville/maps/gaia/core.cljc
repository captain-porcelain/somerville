;; http://experilous.com/1/blog/post/procedural-planet-generation
(ns somerville.maps.gaia.core
  (:require
    [somerville.geometry.point :as point]
    [somerville.geometry.line :as line]
    [somerville.geometry.triangle :as triangle]
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.sphere :as sphere]
    [somerville.geometry.delaunay :as delaunay]
    [somerville.geometry.projection.stereographic :as proj]
    [clojure.set :as s]))


;;====================================================================================================
;; Mesh Helpers

(defn line-to-sphere
  "Map the points of a line onto a sphere."
  [l]
  (line/line (proj/to-sphere (:p1 l)) (proj/to-sphere (:p2 l))))

(defn triangle-to-sphere
  "Map the points of a triangle onto a sphere."
  [t]
  (triangle/triangle (proj/to-sphere (:p1 t)) (proj/to-sphere (:p2 t)) (proj/to-sphere (:p3 t))))

(defn polygon-to-sphere
  "Map the points of a polygon onto a sphere."
  [p]
  (polygon/polygon (map line-to-sphere (:lines p))))

(defn to-voronoi
  "Create voronoi for points on a sphere."
  [points]
  (let [projected (map #(point/scale % 10) (map proj/to-plane points))
        d (delaunay/delaunay projected)
        cells (map :polygon (delaunay/to-cells (delaunay/voronoi d)))]
    (map polygon-to-sphere (map #(polygon/scale % 0.1) cells))))

(defn to-delaunay
  "Create delaunay for points on a sphere."
  [points]
  (let [projected (map #(point/scale % 10) (map proj/to-plane points))
        triangles (map :t (:triangles (delaunay/delaunay projected)))]
    (map triangle-to-sphere (map #(triangle/scale % 0.1) triangles))))


;;====================================================================================================
;; Mesh Building

(def default-config
  {:points 400
   :scale 200
   :random 0.05})

(defn fibonacci
  "Create a list of lines that represent a cube."
  [config]
  (map #(point/scale % (:scale config)) (sphere/fibonacci (:points config))))

(defn delaunay
  "Create a list of triangles for a delaunay of a fibonacci sphere."
  [config]
  (map #(triangle/scale % (:scale config)) (to-delaunay (sphere/fibonacci (:points config)))))

(defn voronoi
  "Create a list of lines for a voronoi of a fibonacci sphere."
  [config]
  (map #(line/scale % (:scale config)) (to-voronoi (sphere/fibonacci (:points config)))))


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

