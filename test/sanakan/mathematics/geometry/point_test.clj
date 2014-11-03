(ns sanakan.mathematics.geometry.point-test
  (:require
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.commons :as c])
  (:use midje.sweet))

(def p1 (p/point -1 -2))
(fact (:x p1) => -1)
(fact (:y p1) => -2)

(def p2 (p/point 3 5))
(def midp1p2 (p/midpoint p1 p2))
(fact (:x midp1p2) => 1)
(fact (:y midp1p2) => 3/2)

(fact (p/slope p1 p2) => 7/4)
(fact (p/slope (p/point 0 0) (p/point 1 0)) => 0)
(fact (p/slope (p/point 0 0) (p/point 1 1)) => 1)

(fact (p/angle (p/point 0 0) (p/point 1 0) (p/point 1 0)) => 0.0)
(fact (c/close-to (p/angle (p/point 0 0) (p/point 1 0) (p/point 0 1)) (/ java.lang.Math/PI 2)) => true)
