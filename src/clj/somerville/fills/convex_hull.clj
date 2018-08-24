;; Implementation of Quick Hull (https://en.wikipedia.org/wiki/Quickhull)
(ns somerville.fills.convex-hull
  (:require
    [clojure.set :as s]
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.triangle :as t]
    [somerville.geometry.polygon :as poly]))

(defn cut-line
  "Get line between the points with minimun x and maximum x."
  [points]
  (let [sorted (sort-by p/x points)]
    (l/line (first sorted) (last sorted))))

(defn above?
  "Check if a point is above a line."
  [point line]
  (let [ly (l/solve-line-at line (p/x point))]
    (cond
      (nil? ly) :on
      (c/close-to (p/y point) ly) :on
      (> (p/y point) ly) :above
      (< (p/y point) ly) :below)))

(defn partition-points
  "Partition points by given line."
  [points line]
  (let [classified (map #(vector (above? % line) %) points)]
    {:above (map second (filter #(= :above (first %)) classified))
     :below (map second (filter #(= :below (first %)) classified))}))

(defn furthest-point
  "Find the point furthest from the given line."
  [line points]
  (second (last (sort-by first (map #(vector (t/height (t/triangle (:p1 line) (:p2 line) %)) %) points)))))

(defn filter-inside-triangle
  "Remove all points that are inside the triangle."
  [points triangle]
  (filter #(poly/point-inside-polygon? triangle %) points))

(defrecord HullSegment [line points side-key]
  c/Printable
  (c/out [this i] (str (c/indent i) "HullSegment of line\n"
                              (c/out line (inc i)) " with " (count points) " points " side-key "."))
  (c/out [this] (c/out this 0)))

(defn hull-segment
  "Create new HullSegment."
  [line points side-key]
  (HullSegment. line points side-key))

(defn split-segment
  "Split a hull segment in two."
  [segment]
  (let [fp (furthest-point (:line segment) (:points segment))
        triangle (t/triangle (:p1 (:line segment)) (:p2 (:line segment)) fp)
        remaining (filter-inside-triangle (:points segment) triangle)
        line1  (l/line (:p1 (:line segment)) fp)
        line2  (l/line fp (:p2 (:line segment)))
        above1 ((:side-key segment) (partition-points remaining line1))
        above2 ((:side-key segment) (partition-points remaining line2))]
    [(hull-segment line1 above1 (:side-key segment)) (hull-segment line2 above2 (:side-key segment))]))

(defn split-segments
  [segments]
  (reduce
    concat
    (map
      #(if (= 0 (count (:points %)))
         (list %)
         (split-segment %))
      segments)))

(defn process-side
  [segment]
  (loop
    [segments (list segment)]
    (if (every? #(= 0 (count (:points %))) segments)
      segments
      (recur (split-segments segments)))))

(defn quick-hull
  "Calculate hull for set of points."
  [points]
  (let [cl (cut-line points)
        partitions (partition-points points cl)]
    (poly/polygon
      (map :line
        (concat
          (process-side (hull-segment cl (:above partitions) :above))
          (process-side (hull-segment (l/line (:p2 cl) (:p1 cl)) (:below partitions) :below)))))))

