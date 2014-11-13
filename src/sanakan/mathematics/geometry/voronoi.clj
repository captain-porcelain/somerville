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
(defrecord Intersection [intersection bisector1 bisector2 angle]
  c/Printable
  (c/out [this i] (str (c/indent i) "Intersections between\n"
                       (c/out bisector1 (+ i 2))
                       (c/indent i) "and\n"
                       (c/out bisector2 (+ i 2))
                       (c/indent i) "is " (c/out intersection) " with angle " angle "\n"))
  (c/out [this] (c/out this 0)))

(defn intersect
  "Calculate all intersections of one bisector with a set of bisectors."
  [bisector bisectors]
  (filter
    #(not (nil? %))
    (for [l1 bisectors]
      (when-not (= bisector l1)
        (let [i (l/intersect (:line bisector) (:line l1))]
          (if (nil? i)
            nil
            (Intersection.  i bisector l1 (p/angle (:p1 bisector) (p/point 0 0) i))))))))

(defn intersect-bisectors
  "Given a point and its bisectors calculates all intersections of the bisectors."
  [p bbox]
  (let [bisectors (concat (:bisectors p) bbox)]
    (assoc p :intersections (sort-by :angle (reduce concat (map #(intersect % bisectors) bisectors))))))

(defn count-intersections
  "Count the intersections with bisectors that are more relevant than the given one."
  [point intersection bisectors]
  (let [dist (p/distance point intersection)
        distances (map
                    #(hash-map :dp (p/distance point %) :di (p/distance intersection %))
                    (l/cuts (l/line point intersection) bisectors))
        closer (filter #(and (< (:dp %) dist) (< (:di %) dist)) distances)]
    (count closer)))

(defn relevant?
  "Check if the intersection is relevant for the voronoi cell."
  [point intersection bisectors]
  (= 0 (count-intersections point intersection bisectors)))

(defn bounding-box
  "Create lines that describe the bounding box for the voronoi"
  [bx1 by1 bx2 by2]
  (list
    (Bisector. (p/point bx1 (- by1 1)) (p/point bx1 (+ by1 1)) (l/line (p/point bx1 by1) (p/point bx2 by1)))
    (Bisector. (p/point (- bx1 1) by1) (p/point (+ bx1 1) by1) (l/line (p/point bx1 by1) (p/point bx1 by2)))
    (Bisector. (p/point (- bx2 1) by2) (p/point (+ bx2 1) by2) (l/line (p/point bx2 by1) (p/point bx2 by2)))
    (Bisector. (p/point bx1 (- by2 1)) (p/point bx1 (+ by2 1)) (l/line (p/point bx1 by2) (p/point bx2 by2)))))

;;-----------------------------------------------------------------------------
;; Main section for creating voronois.

(defrecord Cell [point lines]
  c/Printable
  (c/out [this i] (str (c/indent i) "Voronoicell for the " (c/out point) " has the following lines:\n"
                       (reduce str (interpose "\n" (for [l lines] (c/out l (+ i 2)))))
                       "\n"))
  (c/out [this] (c/out this 0)))

(defrecord Voronoi [points cells bx1 by1 bx2 by2]
  c/Printable
  (c/out [this i] (str (c/indent i) "Voronoi for the points\n"
                       (reduce str (interpose "\n" (for [p points] (c/out (:point p) (+ i 2)))))
                       "\n\nconsists of bisectors\n"
                       (reduce str (for [p points] (reduce str (for [b (:bisectors p)] (c/out b (+ i 2))))))
                       "\nwith intersections at\n"
                       (reduce str (for [p points] (reduce str (for [b (:intersections p)] (c/out b (+ i 2))))))
                       "\nwith cells\n"
                       (reduce str (for [cell cells] (c/out cell (+ i 2))))))
  (c/out [this] (c/out this 0)))

(defn cell-corners
  "Calculate voronoi cell corner points from intersected bisectors."
  [site]
  (distinct
    (map :intersection
         (filter
           #(relevant? (:point site) (:intersection %) (map :line (:bisectors site)))
           (:intersections site)))))

(defn connect-cell
  "Connect a list of points into a list of lines from point to point."
  [cell]
  (map #(l/line %1 %2) cell (concat (rest cell) (list (first cell)))))

(defn cell
  "Create cell instance from point and edges of cell."
  [point lines]
  (Cell. point lines))

(defn voronoi
  "Calculate a set of voronoi cells when given a set of points and a bounding box."
  [points bx1 by1 bx2 by2]
  (let [bbox (bounding-box bx1 by1 bx2 by2)
        intersected (map #(intersect-bisectors % bbox) (bisectors points))
        cell-edges (map connect-cell (map cell-corners intersected))
        cells (map cell points cell-edges)]
    (Voronoi. intersected cells bx1 by1 bx2 by2)))
