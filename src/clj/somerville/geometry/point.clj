(ns somerville.geometry.point
  (:require
    [somerville.geometry.commons :as c]))

;; define a two dimensional point
(defrecord Point2 [x y]
  java.lang.Comparable
  (java.lang.Comparable/compareTo
    [this other]
    (if
      (= (:x this) (:x other))
      (.compareTo (:y this) (:y other))
      (.compareTo (:x this) (:x other))))
  c/Printable
  (c/out [this i] (str (c/indent i) "Point (" x "," y ")"))
  (c/out [this] (c/out this 0)))

;; define a three dimensional point
(defrecord Point3 [x y z]
  c/Printable
  (c/out [this i] (str (c/indent i) "Point (" x "," y "," z ")"))
  (c/out [this] (c/out this 0)))

(defn point
  "Create a point in either 2 or 3 dimensions."
  ([x y]
   (Point2. x y))
  ([x y z]
   (Point3. x y z)))

(defn midpoint
  "Get the midpoint of two points."
  [p1 p2]
  (point (/ (+ (:x p1) (:x p2)) 2) (/ (+ (:y p1) (:y p2)) 2)))

(defn slope
  "Get the slope of two points."
  [p1 p2]
  (let [dx (- (:x p2) (:x p1))
        dy (- (:y p2) (:y p1))]
    (if (or (= dx 0.0) (= dx 0)) nil (/ dy dx))))

(defn subtract
  "Subtract second point from first"
  [p1 p2]
  (point (- (:x p1) (:x p2)) (- (:y p1) (:y p2))))

(defn distance
  "Calculate distance between two points."
  [p1 p2]
  (let [dx (- (:x p1) (:x p2))
        dy (- (:y p1) (:y p2))]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn quadrant
  "Get the quadrant a point is in."
  [p]
  (if (nil? p)
    0
    (cond (and (=  (:x p) 0) (=  (:y p) 0)) 1
          (and (>  (:x p) 0) (>= (:y p) 0)) 1
          (and (<= (:x p) 0) (>  (:y p) 0)) 2
          (and (<  (:x p) 0) (<= (:y p) 0)) 3
          (and (>= (:x p) 0) (<  (:y p) 0)) 4)))

(defn angle-to-x
  "Calculate the angle that is opened by the lines from (0,0) to (1,0) and (0,0) to p."
  [p]
  (let [p1 (point 0 0)
        p2 (point 1 0)
        p3 p
        d12 (distance p1 p2)
        d13 (distance p1 p3)
        d23 (distance p2 p3)
        t1 (* 2 d12 d13)
        t (if (= 0 t1) 0 (/ (- (+ (* d12 d12) (* d13 d13)) (* d23 d23)) t1))
        a (java.lang.Math/acos t)
        a (if (< (:y p) 0) (- (* 2 java.lang.Math/PI) a) a)]
    a))

(defn angle
  "Calculate the angle that is opened by the lines from p1 to p2 and p1 to p3."
  [p1 p2 p3]
  (- (angle-to-x (subtract p3 p1)) (angle-to-x (subtract p2 p1))))

(defn point-at
  "Given a point find another one in dist at angle."
  [p angle dist]
  (point
    (+ (:x p) (* dist (java.lang.Math/cos angle)))
    (+ (:y p) (* dist (java.lang.Math/sin angle)))))
