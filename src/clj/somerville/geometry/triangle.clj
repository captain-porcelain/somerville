(ns somerville.geometry.triangle
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [taoensso.timbre :as log]))


(defrecord Triangle [p1 p2 p3]
  c/Printable
  (c/out [this i] (str (c/indent i) "Triangle of points\n"
                              (c/out p1 (inc i)) "\n" (c/out p2 (inc i)) "\n" (c/out p3 (inc i))))
  (c/out [this] (c/out this 0)))

(defn triangle
  [p1 p2 p3]
  (Triangle. p1 p2 p3))

(defn area
  [t]
  (/
    (Math/abs
      (-
       (* (- (:x (:p1 t)) (:x (:p3 t)))
          (- (:y (:p2 t)) (:y (:p1 t))))
       (* (- (:x (:p1 t)) (:x (:p2 t)))
          (- (:y (:p3 t)) (:y (:p1 t))))))
    2))

(defn height
  "Get the height of the triangle. Considers the line from p1 to p2 the base."
  [t]
  (/ (* 2 (area t)) (p/distance (:p1 t) (:p2 t))))
