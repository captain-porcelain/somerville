(ns somerville.geometry.polygon
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [taoensso.timbre :as log]))

;; Define a polygon as a list of lines and an optional center.
;; This polygon is specifically used for calculating visibility.
(defrecord Polygon2 [lines center]
  c/Printable
  (c/out [this i] (str (c/indent i) "Polygon of lines:\n" (clojure.string/join "\n" (map #(if (nil? %) "NIL" (c/out % (inc i))) lines))))
  (c/out [this] (c/out this 0)))

(defn polygon
  "Get a line from two points."
  ([lines center]
   (Polygon2. lines center))
  ([lines]
   (Polygon2. lines nil)))

(defn from-points
  "Create a polygon from a list of points."
  ([points center]
   (polygon (map #(l/line %1 %2) points (concat (rest points) (list (first points)))) center))
  ([points]
   (from-points points nil)))

(defn to-points
  "Create list of points in polygon."
  [poly]
  (map :p1 (:lines poly)))

(defn scale
  "Scale polygon by factor."
  [poly factor]
  (from-points (map #(p/scale % factor) (to-points poly))))

;;====================================================================================================================================================
;; General Helpers

(defn intersect
  "Intersect a polygon with a line."
  [polygon line]
  (filter #(not (nil? %)) (map #(l/intersect % line) (:lines polygon))))

(defn intersect-segments
  "Intersect a polygon with a line."
  [polygon line]
  (distinct (sort (filter #(not (nil? %)) (map #(l/intersect-segments % line) (:lines polygon))))))

(defn on-polygon?
  "Check if a point is on some line of a polygon."
  [polygon point]
  (< 0 (count (filter #(l/point-on-segment? % point) (:lines polygon)))))

(defn point-inside-polygon?
  "Check if point is inside the polygon."
  [polygon point]
  (or
    (on-polygon? polygon point)
    (= 0 (count (intersect-segments polygon (l/line (:center polygon) point))))))

(defn shorten-line-int
  "Reduce line to the parts that are inside the given polygon. If the line is not inside the polygon return nil."
  [polygon line]
  (let [intersections (intersect-segments polygon line)]
    (case (count intersections)
      0 (if (and (point-inside-polygon? polygon (:p1 line)) (point-inside-polygon? polygon (:p2 line))) line nil)
      1 (l/line (first (filter #(point-inside-polygon? polygon %) (list (:p1 line) (:p2 line)))) (first intersections))
      2 (l/line (nth intersections 0) (nth intersections 1))
      line)))

(defn shorten-line
  "Reduce line to the parts that are inside the given polygon. If the line is not inside the polygon return nil."
  [polygon line]
  (let [s (shorten-line-int polygon line)]
    (when-not (= (:p1 s) (:p2 s)) s)))

(defn- signed-area-impl
  "Worker function for recursively calculating the signed area of a 2d polygon."
  [points area]
  (if (< (count points) 2)
    (/ area 2)
    (let [xiyi1 (* (:x (first points)) (:y (second points)))
          xi1yi (* (:x (second points)) (:y (first points)))
          a (- xiyi1 xi1yi)]
      (recur (rest points) (+ area a)))))

(defn signed-area
  "Calculate signed area of a 2d polygon."
  [poly]
  (let [points (to-points poly)]
    (signed-area-impl (conj points (first points)) 0)))

;; The algorithm was found at http://paulbourke.net/geometry/polyarea/
(defn- centroid-x-y
  "Calculate the centroid of a 2d polygon."
  [points cx cy]
  (if (< (count points) 2)
    [cx cy]
    (let [xiyi1 (* (:x (first points)) (:y (second points)))
          xi1yi (* (:x (second points)) (:y (first points)))
          sub (- xiyi1 xi1yi)
          xixi1 (+ (:x (first points)) (:x (second points)))
          yiyi1 (+ (:y (first points)) (:y (second points)))]
      (recur (rest points) (+ cx (* xixi1 sub)) (+ cy (* yiyi1 sub))))))

(defn centroid-2d
  "calculate the centroid of a 2 dimensional polygon"
  [poly]
  (let [points (to-points poly)
        a (signed-area poly)
        [cx cy] (centroid-x-y (conj points (first points)) 0 0)]
    (p/point (/ cx (* 6 a)) (/ cy (* 6 a)))))

(defn centroid-3d
  "calculate the centroid of a 3 dimensional polygon"
  [polygon]
  (let [points (to-points polygon)
        xs (map :x points)
        ys (map :y points)
        zs (map :z points)]
    (p/point (c/avg xs) (c/avg ys) (c/avg zs))))


;;====================================================================================================================================================
;; adding two polygons see https://stackoverflow.com/questions/2667748/how-do-i-combine-complex-polygons

(defn sort-points-on-line
  "Sort points on a line with the start and end points that line."
  [l ps]
  (concat (cons (:p1 l) (sort-by #(p/distance (:p1 l) %) ps)) (list (:p2 l))))

(defn outline-split
  "Given a line and points on that line create a map holding all points as keys
  and a list of one point that is next to that point as values."
  [l ps]
  (loop [ps (sort-points-on-line l ps)
         m {}]
    (if (<= (count ps) 1)
      m
      (recur (rest ps) (assoc m (first ps) (list (first (rest ps))))))))

(defn outline-graph
  "Create a graph that reflects the outline of two polygons with all intersections between them."
  [poly1 poly2]
  (apply merge-with concat
         (concat
           (map #(outline-split % (intersect-segments poly2 %)) (:lines poly1))
           (map #(outline-split % (intersect-segments poly1 %)) (:lines poly2)))))

(defn outline-angle-candidates
  "Sort the outline candidates by angle."
  [previous current candidates]
  (sort-by second (map #(vector % (p/angle-dot current previous %)) candidates)))

(defn next-outline-point
  "Get the next point in the outline."
  [previous current candidates]
  (if (nil? previous)
    (first candidates)
    (first (last (outline-angle-candidates previous current candidates)))))

(defn build-outline
  "Iterate through graph to get the outline."
  [graph start]
  (loop [g graph
         p (next-outline-point nil start (get graph start))
         ps (vector start)]
    (if (= start p)
      {:outline (from-points ps) :remaining g}
      (recur (dissoc g p) (next-outline-point (last ps) p (get g p)) (conj ps p)))))

(defn outline
  "Create a polygon that outlines two polygons."
  [poly1 poly2]
  (let [graph (outline-graph poly1 poly2)
        points (concat (to-points poly1) (to-points poly2))
        start (p/low-left points)]
    (:outline (build-outline graph start))))


