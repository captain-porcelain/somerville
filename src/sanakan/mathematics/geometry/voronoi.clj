(ns sanakan.mathematics.geometry.voronoi
  (:require
    [sanakan.mathematics.geometry.geometry :as g]
    [sanakan.mathematics.geometry.line :as l]
    [sanakan.mathematics.geometry.point :as p]))

(defn calculate-bisectors
  "Calculate the bisectors of one point with a list of points."
  [p points]
  (filter
    #(not (nil? %))
      (for [p1 points]
        (when-not (= p p1) (l/bisector p p1)))))

(defn calculate-all-bisectors
  "Calculate all bisectors for a point set."
  [points]
  (for [p1 points]
    {:p p1 :bisectors (calculate-bisectors p1 points)}))

(defn calculate-intersections
  "Calculate all intersections of one line with a set of lines."
  [l lines]
  (filter
    #(not (nil? %))
      (for [l1 lines]
        (when-not (= l l1) (l/intersect l l1)))))

(defn calculate-intersections-for-one-bisector
  "Given one point with its bisectors calculates all intersections of the bisectors."
  [bisectors]
  (for [b bisectors]
    {:bisector b :intersections (calculate-intersections b bisectors)}))

(defn calculate-all-intersections
  "Calculate all intersections for all bisectors."
  [p]
  (assoc p :bisectors (calculate-intersections-for-one-bisector (:bisectors p))))

(defn voronoi
  "Calculate a set of voronoi cells when given a set of points and a bounding box."
  [points bx1 by1 bx2 by2]
  (let [bisectors (calculate-all-bisectors points)
        bisectors (map calculate-all-intersections bisectors)
        ]
    bisectors))
