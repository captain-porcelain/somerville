(ns somerville.geometry.place
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.commons :as c]))

;; Define a plane with three points
(defrecord Plane [p1 p2 p3]
  c/Printable
  (c/out [this i] (str (c/indent i) "Plane through points\n" (c/out p1 (inc i) "\n" (c/out p1 (inc i))) "\n" (c/out p1 (inc i))))
  (c/out [this] (c/out this 0)))

(defn point
  "Create a point in either 2 or 3 dimensions."
  [p1 p2 p3]
   (Plane. p1 p2 p3))

(defn normal
  "Get the normal for the plane"
  [plane]
  (p/cross (p/subtract (:p2 plane) (:p1 plane)) (p/subtract (:p3 plane) (:p1 plane))))

