(ns sanakan.mathematics.geometry.voronoi
  (:require
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.line :as l]
    [sanakan.mathematics.geometry.point :as p]))

;;-----------------------------------------------------------------------------
;; Section for bisector handling specific to voronoi calculation.

;; Define a structure for a bisector given by the two points and the bisecting line.
(defrecord Bisector [p1 p2 line]
  c/Printable
  (c/out [this i] (str (c/indent i) "Bisector between " (c/out p1) " and " (c/out p2) " is\n"
                       (c/out line (+ i 2)) "\n"))
  (c/out [this] (c/out this 0)))

(defn bisectors
  "Calculate all bisectors for a list of points.
  For each point a map with that point and the bisectors with the other points is returned."
  [points]
  (for [p1 points]
    {:point p1
     :bisectors (filter
                  #(not (nil? %))
                  (for [p2 points]
                    (when-not (= p1 p2) (Bisector. p1 p2 (l/bisector p1 p2)))))}))

;;-----------------------------------------------------------------------------
;; Section for intersection handling specific to voronoi calculation.

;; Define a structure for intersections of bisectors.
(defrecord Intersection [intersection bisector1 bisector2]
  c/Printable
  (c/out [this i] (str (c/indent i) "Intersections between\n"
                       (c/out bisector1 (+ i 2))
                       (c/indent i) "and\n"
                       (c/out bisector2 (+ i 2))
                       (c/indent i) "is " (c/out intersection) "\n"))
  (c/out [this] (c/out this 0)))

(defn intersect
  "Calculate all intersections of one bisector with a set of bisectors."
  [bisector bisectors]
  (filter
    #(not (nil? %))
    (for [l1 bisectors]
      (when-not (= bisector l1)
        (Intersection. (l/intersect (:line bisector) (:line l1)) bisector l1)))))

(defn intersect-bisectors
  "Given a point and its bisectors calculates all intersections of the bisectors."
  [p]
  (assoc p :intersections (reduce concat (map #(intersect % (:bisectors p)) (:bisectors p)))))

;;-----------------------------------------------------------------------------
;; Main section for creating voronois.

(defrecord Voronoi [points bx1 by1 bx2 by2]
  c/Printable
  (c/out [this i] (str (c/indent i) "Voronoi for the points\n"
                       (reduce str (interpose "\n" (for [p points] (c/out (:point p) (+ i 2)))))
                       "\n\nconsists of bisectors\n"
                       (reduce str (for [p points] (reduce str(for [b (:bisectors p)] (c/out b (+ i 2))))))
                       "\nwith intersections at\n"
                       (reduce str (for [p points] (reduce str(for [b (:intersections p)] (c/out b (+ i 2))))))))
  (c/out [this] (c/out this 0)))

(defn voronoi
  "Calculate a set of voronoi cells when given a set of points and a bounding box."
  [points bx1 by1 bx2 by2]
  (let [intersected (map intersect-bisectors (bisectors points))]
    (Voronoi. intersected bx1 by1 bx2 by2)))
