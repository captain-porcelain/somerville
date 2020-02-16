(ns somerville.geometry.icosahedron
  (:require
    [somerville.geometry.commons :as gcommons]
    [somerville.geometry.point :as point]
    [somerville.geometry.line :as line]
    [somerville.geometry.triangle :as triangle]
    [somerville.geometry.polygon :as polygon]
    [clojure.set :as s]))


(def icosahedron-corners
  (list
    (point/point -0.26286500  0.0000000   0.42532500)
    (point/point  0.26286500  0.0000000   0.42532500)
    (point/point -0.26286500  0.0000000  -0.42532500)
    (point/point  0.26286500  0.0000000  -0.42532500)
    (point/point  0.0000000   0.42532500  0.26286500)
    (point/point  0.0000000   0.42532500 -0.26286500)
    (point/point  0.0000000  -0.42532500  0.26286500)
    (point/point  0.0000000  -0.42532500 -0.26286500)
    (point/point  0.42532500  0.26286500  0.0000000)
    (point/point -0.42532500  0.26286500  0.0000000)
    (point/point  0.42532500 -0.26286500  0.0000000)
    (point/point -0.42532500 -0.26286500  0.0000000)))



(defn distances
  "Get a sorted list of distances to all points in ps, excluding p itself."
  [p ps]
  (rest
    (sort-by first
             (map #(vector (point/distance p %) %)
                  ps))))

(defn all-closest
  "Find the points in ps that are closest to a point p."
  [p ps]
  (let [dists (distances p ps)
        ref-dist (first (first dists))]
    (map second (take-while #(gcommons/close-to ref-dist (first %)) dists))))

(defn lines-to
  "Create lines between p and those points in ps."
  [p ps]
  (map
    #(apply line/line (sort (list p %)))
    ps))

(defn lines
  "Create lines from a set of points. Each point is connected to all those that are closest to it."
  [points]
  (distinct (sort (reduce concat (map #(lines-to % (all-closest % points)) points)))))

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

(defn subdivide
  "Subdivide each triangle in a list."
  [ts]
  (into '() (apply s/union (map #(into #{} (triangle/subdivide %)) ts))))

(defn polygonate
  [p ts]
  (let [affected (filter #(or (= (:p1 %) p (= (:p2 %) p)) (= (:p3 %) p)) ts)
        centers (map #(point/scale (point/add (:p1 %) (point/add (:p2 %) (:p3 %))) 0.3333) affected)]
    (polygon/from-points centers)))

(defn triangle-points
  "Get all unique points from list of triangles."
  [ts]
  (apply s/union (map #(into #{} (list (:p1 %) (:p2 %) (:p3 %))) ts)))

(defn translate-surface
  "Translate triangle surface to polygon for each triangle point."
  [ts]
  (map #(polygonate % ts) (triangle-points ts)))

(defn icosahedron
  "Create a set of triangles that represent an icosahedron."
  [scale]
  (triangles (map #(line/scale % scale) (lines icosahedron-corners)) false))
