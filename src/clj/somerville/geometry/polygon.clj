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
