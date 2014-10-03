(ns sanakan.mathematics.geometry.point
  (:require
    [clojure.core.typed :refer [ann AnyInteger]]))

;; define a two dimensional point
(defstruct point2 :x :y)
;; define a three dimensional point
(defstruct point3 :x :y :z)

(defn point
  "Create point in either 2 or 3 dimensions."
  ([x y]
   (struct-map point2 :x x :y y))
  ([x y z]
   (struct-map point3 :x x :y y :z z)))

(defn midpoint
  "Get the midpoint of two points."
  [p1 p2]
  (struct-map point2 :x (/ (+ (:x p1) (:x p2)) 2) :y (/ (+ (:y p1) (:y p2)) 2)))

(defn slope
  "Get the slope of two points."
  [p1 p2]
  (let [dx (- (:x p2) (:x p1))
        dy (- (:y p2) (:y p1))]
    (if (= dx 0) 999999 (/ dy dx))))
