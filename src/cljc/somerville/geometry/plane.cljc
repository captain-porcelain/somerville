(ns somerville.geometry.plane
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.commons :as c]))

;; Define a plane with three points
(defrecord Plane [p1 p2 p3 n]
  c/Printable
  (c/out [this i] (str (c/indent i) "Plane through points\n" (c/out p1 (inc i)) "\n"
                       (c/out p2 (inc i)) "\n"
                       (c/out p3 (inc i)) "\n"
                       "and normal " (c/out n)))
  (c/out [this] (c/out this 0)))

(defn normal
  "Get the normal for the plane"
  [p1 p2 p3]
  (p/cross (p/subtract p2 p1) (p/subtract p3 p1)))

(defn plane
  "Create a point in either 2 or 3 dimensions."
  [p1 p2 p3]
   (Plane. p1 p2 p3 (normal p1 p2 p3)))

(defn intersect
  "Find point where line intersects plane.
  See https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection"
  [plane line]
  (let [n (:n plane)
        p0 (:p1 plane)
        l0 (:p1 line)
        l (p/subtract (:p2 line) (:p1 line))
        upper (p/dot (p/subtract p0 l0) n)
        lower (p/dot l n)]
    (if (c/close-to 0 lower)
      nil
      (p/add (p/scale l (/ upper lower)) l0))))
