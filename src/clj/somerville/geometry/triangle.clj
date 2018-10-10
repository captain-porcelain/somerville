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
  (line/intersect (line/bisector (:p1 t) (:p2 t)) (line/bisector (:p2 t) (:p3 t))))

(defn circumcircle
  "Calculate the circumcircle of a triangle "
  [t]
  (circle/circle
    (circumcenter t)
    (let [a (sc/abs (point/distance (:p1 t) (:p2 t)))
          b (sc/abs (point/distance (:p2 t) (:p3 t)))
          c (sc/abs (point/distance (:p3 t) (:p1 t)))]
      (/ (* a b c) (Math/sqrt (/ (* (+ a b c) (- (+ b c) a)) (* (- (+ c a) b) (- (+ a b) c))))))))

