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
      (c/compareTo (:y this) (:y other))
      (c/compareTo (:x this) (:x other))))
  c/Printable
  (c/out [this i] (str (c/indent i) "Point (" x "," y ")"))
  (c/out [this] (c/out this 0)))

;; define a three dimensional point
(defrecord Point3 [x y z]
  java.lang.Comparable
  (java.lang.Comparable/compareTo
    [this other]
    (if
      (= (:x this) (:x other))
      (if
        (= (:y this) (:y other))
        (c/compareTo (:z this) (:z other))
        (c/compareTo (:y this) (:y other)))
      (c/compareTo (:x this) (:x other))))
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
  (if (and (not (nil? (:z p1))) (not (nil? (:z p2))))
    (point (/ (+ (:x p1) (:x p2)) 2) (/ (+ (:y p1) (:y p2)) 2) (/ (+ (:z p1) (:z p2)) 2))
    (point (/ (+ (:x p1) (:x p2)) 2) (/ (+ (:y p1) (:y p2)) 2))))

(defn slope
  "Get the slope of two points."
  [p1 p2]
  (let [dx (- (:x p2) (:x p1))
        dy (- (:y p2) (:y p1))]
    (if (or (= dx 0.0) (= dx 0)) nil (/ dy dx))))

(defn subtract
  "Subtract second point from first"
  [p1 p2]
  (if (or (nil? p1) (nil? p2))
    nil
    (if (and (not (nil? (:z p1))) (not (nil? (:z p2))))
      (point (- (:x p1) (:x p2)) (- (:y p1) (:y p2)) (- (:z p1) (:z p2)))
      (point (- (:x p1) (:x p2)) (- (:y p1) (:y p2))))))

(defn distance
  "Calculate distance between two points."
  [p1 p2]
  (if (or (nil? p1) (nil? p2))
    ;(throw (Exception. (str "point is nil")))
    Long/MAX_VALUE
    (let [dx (- (:x p1) (:x p2))
          dy (- (:y p1) (:y p2))
          dz (- (get p1 :z 0) (get p2 :z 0))]
      (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))))))

(defn normalize
  "Normalize point (as vector) to length 1 if possible."
  [p]
  (let [z (point 0 0 0)
        d (distance p z)]
    (if (c/close-to 0 d)
      p
      (point (/ (:x p) d) (/ (:y p) d) (/ (get p :z 0) d)))))

(defn cross
  "Calculate cross product between two 3D vectors."
  [p1 p2]
  (point
    (- (* (:y p1) (:z p2)) (* (:z p1) (:y p2)))
    (- (* (:z p1) (:x p2)) (* (:x p1) (:z p2)))
    (- (* (:x p1) (:y p2)) (* (:y p1) (:x p2)))))

(defn dot
  "Calculate dot product between two points."
  [p1 p2]
  (+ (* (:x p1) (:x p2)) (* (:y p1) (:y p2))  (* (get p1 :z 0) (get p2 :z 0))))

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
        a (Math/acos t)
        a (if (< (:y p) 0) (- (* 2 Math/PI) a) a)]
    a))

(defn angle
  "Calculate the angle that is opened by the lines from p1 to p2 and p1 to p3."
  [p1 p2 p3]
  (- (angle-to-x (subtract p3 p1)) (angle-to-x (subtract p2 p1))))

(defn angle-pos
  "Calculate the angle that is opened by the lines from p1 to p2 and p1 to p3. No negative results."
  [p1 p2 p3]
  (try
    (let [a (angle p1 p2 p3)]
      (if (< a 0) (+ (* 2 Math/PI) a) a))
    (catch Exception e (dorun (println (str "Error calculating angle between:\n" (c/out p1 1) "\n" (c/out p2 1) "\n" (c/out p3 1)))))))

(defn point-at
  "Given a point find another one in dist at angle."
  [p angle dist]
  (point
    (+ (:x p) (* dist (Math/cos angle)))
    (+ (:y p) (* dist (Math/sin angle)))))

(defn close?
  "Check if two points are close together."
  [p1 p2]
  (and
    (c/close-to (:x p1) (:x p2))
    (c/close-to (:y p1) (:y p2))
    (c/close-to (get p1 :z 0) (get p2 :z 0))))

(defn add
  "Treat points as vectors and add them."
  [p1 p2]
  (if (or (nil? p1) (nil? p2))
    nil
    (point (+ (:x p1) (:x p2)) (+ (:y p1) (:y p2)) (+ (get p1 :z 0) (get p2 :z 0)))))

(defn scale
  "Treat point as vector and scale it by factor."
  [p factor]
  (point (* factor (:x p)) (* factor (:y p)) (* factor (get p :z 0))))

(defn linear-combination
  "Find values a b such that v = a * s + b * t."
  [v s t]
  (let [bupper (- (* (:y v) (:x s)) (* (:x v) (:y s)))
        blower (- (* (:y t) (:x s)) (* (:y s) (:x t)))
        b (/ bupper blower)
        a (/ (- (:x v) (* b (:x t))) (:x s))]
    [a b]))

(defn slerp
  "Spheric linear interpolation between two points at parameter t."
  [p1 p2 t]
  (let [np1 (normalize p1)
        np2 (normalize p2)
        dot (dot np1 np2)
        dot (if (< dot 0) (* -1 dot) dot)
        np1 (if (< dot 0) (scale np1 -1) np1)
        theta0 (Math/acos dot)
        theta (* t theta0)
        s0 (- (Math/cos theta) (* dot (/ (Math/sin theta) (Math/sin theta0))))
        s1 (/ (Math/sin theta) (Math/sin theta0))]
    (add (scale p1 s0) (scale p2 s1))))


