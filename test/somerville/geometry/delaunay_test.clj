(ns somerville.geometry.delaunay-test
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.delaunay :as d]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l])
  (:use clojure.test))

(def p1 (p/point 1 1))
(def p2 (p/point 3 7))
(def p3 (p/point 4 5))
(def p4 (p/point 10 9))
(def p5 (p/point 2 11))
(def p6 (p/point 5 1))
(def points (list p3 p4 p5 p6))

(deftest maxima
  (is (= 10 (d/max-val points :x)))
  (is (= 11 (d/max-val points :y))))

(deftest bounds
  (let [bp1 (p/point 0   0)
        bp2 (p/point 0  22)
        bp3 (p/point 20  0)
        bt (d/bounding-triangle points)]
    (is (= bp1 (:p1 (:t bt))))
    (is (= bp2 (:p2 (:t bt))))
    (is (= bp3 (:p3 (:t bt))))))

(deftest adding
  (let [bt (d/bounding-triangle points)
        added (d/add-point (list bt) p1)]
    (dorun (println added))))
