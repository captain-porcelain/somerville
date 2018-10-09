(ns somerville.geometry.delaunay-test
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.delaunay :as d]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l])
  (:use clojure.test))

(def p1 (p/point 1 1))
(def p2 (p/point 7 9))
(def p3 (p/point 4 5))
(def p4 (p/point 10 9))
(def p5 (p/point 2 11))
(def p6 (p/point 5 1))
(def points (list p3 p4 p5 p6))

(deftest maxima
  (is (= 10 (d/max-val points :x)))
  (is (= 11 (d/max-val points :y))))

(deftest bounds
  (dorun (println (d/bounding-triangle points))))
