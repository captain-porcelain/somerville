(ns somerville.geometry.point-test
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.commons :as c])
  (:use clojure.test))

(def p1 (p/point -1 -2))
(deftest basics
  (is (= (:x p1) -1))
  (is (= (:y p1) -2)))

(def p2 (p/point 3 5))
(def midp1p2 (p/midpoint p1 p2))
(deftest midpoints
  (is (= (:x midp1p2) 1))
  (is (= (:y midp1p2) 3/2)))

(deftest slopes
  (is (= (p/slope p1 p2) 7/4))
  (is (= (p/slope (p/point 0 0) (p/point 1 0)) 0))
  (is (= (p/slope (p/point 0 0) (p/point 1 1)) 1)))

(deftest find-quadrant
  (is (= (p/quadrant (p/point  0  0)) 1))
  (is (= (p/quadrant (p/point  1  0)) 1))
  (is (= (p/quadrant (p/point  1  1)) 1))
  (is (= (p/quadrant (p/point  0  1)) 2))
  (is (= (p/quadrant (p/point -1  1)) 2))
  (is (= (p/quadrant (p/point -1  0)) 3))
  (is (= (p/quadrant (p/point -1 -1)) 3))
  (is (= (p/quadrant (p/point  1 -1)) 4)))

(def PI00 0.0)
(def PI05 (/ java.lang.Math/PI 2))
(def PI10 java.lang.Math/PI)
(def PI15 (* 3 (/ java.lang.Math/PI 2)))
(def PI20 (* 2 java.lang.Math/PI))
(def PI175 (* 7 (/ java.lang.Math/PI 4)))

(deftest angles
  (is (= (c/close-to (p/angle-to-x (p/point  1  0)) PI00)  true))
  (is (= (c/close-to (p/angle-to-x (p/point  0  1)) PI05) true))
  (is (= (c/close-to (p/angle-to-x (p/point -1  0)) PI10) true))
  (is (= (c/close-to (p/angle-to-x (p/point  0 -1)) PI15) true))
  (is (= (c/close-to (p/angle-to-x (p/point  5 -5)) PI175) true))

  (is (= (c/close-to (p/angle (p/point 0 0) (p/point 1 0) (p/point 1 0)) PI00) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point 1 0) (p/point 0 1)) PI05) true))

  (is (= (c/close-to (p/angle (p/point 0 0) (p/point  1  0) (p/point  1  0)) PI00) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point  1  0) (p/point  0  1)) PI05) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point  1  0) (p/point -1  0)) PI10) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point  1  0) (p/point  0 -1)) PI15) true))
  (is (= (c/close-to (p/angle (p/point 0 0) (p/point -1  0) (p/point  1  0)) (* -1 PI10)) true)))

  ;(is (= (c/close-to (p/angle (p/point 5 5) (p/point  0  0) (p/point 10  0)) PI05) true))

(deftest distances
  (is (= (c/close-to (p/distance (p/point-at (p/point 0 0) PI00 1) (p/point  1.0  0.0)) 0) true))
  (is (= (c/close-to (p/distance (p/point-at (p/point 0 0) PI05 1) (p/point  0.0  1.0)) 0) true))
  (is (= (c/close-to (p/distance (p/point-at (p/point 0 0) PI10 1) (p/point -1.0  0.0)) 0) true))
  (is (= (c/close-to (p/distance (p/point-at (p/point 0 0) PI15 1) (p/point  0.0 -1.0)) 0) true)))
