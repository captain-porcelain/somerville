(ns somerville.geometry.circle-test
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.circle :as c]
    [somerville.geometry.line :as l])
  (:use clojure.test))

(deftest intersections
  (let [c (c/circle (p/point 0  0) 1)
        l1 (l/line (p/point -2  0) (p/point 2 0))
        l2 (l/line (p/point -2  1) (p/point 2 1))
        l3 (l/line (p/point -2  2) (p/point 2 2))
        l4 (l/line (p/point  2  0) (p/point 4 0))
        l5 (l/line (p/point  0 -2) (p/point 0 2))
        l6 (l/line (p/point  1 -2) (p/point 1 2))
        l7 (l/line (p/point  4 -2) (p/point 4 2))
        i1 (c/intersect-line-segment c l1)
        i2 (c/intersect-line-segment c l2)
        i3 (c/intersect-line-segment c l3)
        i4 (c/intersect-line-segment c l4)
        i5 (c/intersect-line-segment c l5)
        i6 (c/intersect-line-segment c l6)
        i7 (c/intersect-line-segment c l7)]
    (is (= 2 (count i1)))
    (is (= (nth i1 0) (p/point  1.0 0.0)))
    (is (= (nth i1 1) (p/point -1.0 0.0)))
    (is (= 1 (count i2)))
    (is (= (nth i2 0) (p/point  0 1)))
    (is (= 0 (count i3)))
    (is (= 0 (count i4)))
    (is (= 2 (count i5)))
    (is (= (nth i5 0) (p/point  0 1.0)))
    (is (= (nth i5 1) (p/point  0 -1.0)))
    (is (= 1 (count i6)))
    (is (= (nth i6 0) (p/point  1 0.0)))
    (is (= 0 (count i7)))))

(deftest boxing
  (let [circle (c/circle (p/point 0 0) 1)
        box (c/outer-box circle)]
    (= (p/point -1 -1) (:p1 box))
    (= (p/point -1  1) (:p2 box))
    (= (p/point  1 -1) (:p3 box))
    (= (p/point  1  1) (:p4 box))))
