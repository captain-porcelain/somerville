(ns somerville.geometry.triangle
  (:require
    [somerville.commons :as sc]
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as point]
    [somerville.geometry.line :as line]
    [somerville.geometry.circle :as circle]
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
  "Calculate the area of a 2d triangle."
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
  (/ (* 2 (area t)) (point/distance (:p1 t) (:p2 t))))

(defn subdivide
  "Create 4 new triangles from one triangle, each havin half the side length.
  Works on 3d triangles and utilizes spherical interpolation for the new points."
  [t]
  (let [m1 (point/slerp (:p1 t) (:p2 t) 0.5)
        m2 (point/slerp (:p2 t) (:p3 t) 0.5)
        m3 (point/slerp (:p3 t) (:p1 t) 0.5)]
    (list (triangle (:p1 t) m1 m3) (triangle (:p2 t) m2 m1) (triangle (:p3 t) m3 m2) (triangle m1 m2 m3))))

(defn circumcenter
  "Calculate the circumcenter of a triangle "
  [t]
  (let [i1 (line/intersect (line/bisector (:p1 t) (:p2 t)) (line/bisector (:p2 t) (:p3 t)))
        i2 (line/intersect (line/bisector (:p1 t) (:p2 t)) (line/bisector (:p3 t) (:p1 t)))
        i3 (line/intersect (line/bisector (:p2 t) (:p3 t)) (line/bisector (:p3 t) (:p1 t)))]
    (if (nil? i1) (if (nil? i2) i3 i2) i1)))

(defn circumcircle
  "Calculate the circumcircle of a triangle "
  [t]
  (let [center (circumcenter t)]
    (do (when (nil? center) (log/error (str "Can't calculate circumcenter for " (c/out t)))))
    (circle/circle center (point/distance center (:p1 t)))))

(defn sign
  "Helper for check if point is inside triangle."
  [p1 p2 p3]
  (- (* (- (:x p1) (:x p3)) (- (:y p2) (:y p3))) (* (- (:x p2) (:x p3)) (- (:y p1) (:y p3)))))

(defn inside?
  "Check if point is inside triangle.
  See https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle"
  [t p]
  (let [d1 (sign p (:p1 t) (:p2 t))
        d2 (sign p (:p2 t) (:p3 t))
        d3 (sign p (:p3 t) (:p1 t))
        has-neg (or (< d1 0) (< d2 0) (< d3 0))
        has-pos (or (> d1 0) (> d2 0) (> d3 0))]
    (not (and has-pos has-neg))))

(defn move
  "Move all points of triangle"
  [t p]
  (triangle (point/add (:p1 t) p) (point/add (:p2 t) p) (point/add (:p3 t) p)))

(defn scale
  "Scale all points of triangle"
  [t s]
  (triangle (point/scale (:p1 t) s) (point/scale (:p2 t) s) (point/scale (:p3 t) s)))

