(ns sanakan.mathematics.geometry.point
  (:require
    [sanakan.mathematics.geometry.commons :as c]
    [clojure.math.numeric-tower :as nt]
    [clojure.core.typed :refer [ann AnyInteger]]))

;; define a two dimensional point
(defrecord Point2 [x y]
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
    (if (= dx 0) nil (/ dy dx))))

(defn distance
  "Calculate distance between two points."
  [p1 p2]
  (let [;tmp (dorun (println (str "distance between "p1 " and " p2)))
		dx (- (:x p1) (:x p2))
        dy (- (:y p1) (:y p2))]
    (nt/sqrt (+ (* dx dx) (* dy dy)))))

(defn angle
  "Calculate the angle that is opened by the lines from p1 to p2 and p1 to p3."
  [p1 p2 p3]
  (let [d12 (distance p1 p2)
        d13 (distance p1 p3)
        d23 (distance p2 p3)
        t1 (* 2 d12 d13)
        t (if (= 0 t1) 0 (/ (- (+ (* d12 d12) (* d13 d13)) (* d23 d23)) t1))]
    (java.lang.Math/acos t)))
