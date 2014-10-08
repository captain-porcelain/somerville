(ns sanakan.mathematics.geometry.voronoi
  (:require
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.line :as l]
    [sanakan.mathematics.geometry.point :as p]))

;; Define a structure for a bisector given by the two points and the bisecting line.
(defrecord Bisector [p1 p2 line]
  c/Printable
  (c/out [x] (str "Bisector " p1 " and " p2 " is " line)))

(defn bisectors
  "Calculate all bisectors for a list of points.
  For each point a map with that point and the bisectors with the other points is returned."
  [points]
  (for [p1 points]
    {:p p1
     :bisectors (filter
                  #(not (nil? %))
                  (for [p2 points]
                    (when-not (= p1 p2) (Bisector. p1 p2 (l/bisector p1 p2)))))}))

;; Define a structure for intersections of bisectors.
(defrecord Intersection [intersection bisector1 bisector2]
  c/Printable
  (c/out [x] (str "Intersection " (c/out bisector1) " and " (c/out bisector1) " is " intersection)))

(defn intersect
  "Calculate all intersections of one bisector with a set of bisectors."
  [bisector bisectors]
  (filter
    #(not (nil? %))
    (for [l1 bisectors]
      (when-not (= bisector l1)
        (Intersection. (l/intersect (:line bisector) (:line l1)) bisector l1)))))

(defn intersections
  "Given a point and its bisectors calculates all intersections of the bisectors."
  [p]
  (reduce concat (map #(intersect % (:bisectors p)) (:bisectors p))))

(defn intersect-bisectors
  "Given a point and its bisectors calculates all intersections of the bisectors."
  [p]
  (assoc p :intersections (intersections p)))

(defn voronoi
  "Calculate a set of voronoi cells when given a set of points and a bounding box."
  [points bx1 by1 bx2 by2]
  (let [intersected (map intersect-bisectors (bisectors points))
        ]
    intersected))
