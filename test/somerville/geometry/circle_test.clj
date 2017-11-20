(ns somerville.geometry.circle-test
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.circle :as c]
    [somerville.geometry.line :as l])
  (:use clojure.test))

(deftest intersections
  (testing "Two intersections"
    (let [c (c/circle (p/point 0  0) 1)
          l1 (l/line (p/point -2  0) (p/point 2 0))
          i1 (c/intersect-line-segment c l1)]
      (is (= 2 (count i1)))
      (is (= (nth i1 0) (p/point  1.0 0.0)))
      (is (= (nth i1 1) (p/point -1.0 0.0)))))
  (testing "One intersection"
    (let [c (c/circle (p/point 0  0) 1)
          l2 (l/line (p/point -2  1) (p/point 2 1))
          i2 (c/intersect-line-segment c l2)]
      (is (= 1 (count i2)))
      (is (= (nth i2 0) (p/point  0 1)))))
  (testing "No intersections"
    (let [c (c/circle (p/point 0  0) 1)
          l3 (l/line (p/point -2  2) (p/point 2 2))
          l4 (l/line (p/point  2  0) (p/point 4 0))
          i3 (c/intersect-line-segment c l3)
          i4 (c/intersect-line-segment c l4) ]
      (is (= 0 (count i3)))
      (is (= 0 (count i4)))))
  (testing "Two intersections with vertical line"
    (let [c (c/circle (p/point 0  0) 1)
          l5 (l/line (p/point  0 -2) (p/point 0 2))
          i5 (c/intersect-line-segment c l5)]
      (is (= 2 (count i5)))
      (is (= (nth i5 0) (p/point  0 1.0)))
      (is (= (nth i5 1) (p/point  0 -1.0)))))
  (testing "One intersection with vertical line"
    (let [c (c/circle (p/point 0  0) 1)
          l6 (l/line (p/point  1 -2) (p/point 1 2))
          i6 (c/intersect-line-segment c l6)]
      (is (= 1 (count i6)))
      (is (= (nth i6 0) (p/point  1 0.0)))))
  (testing "No intersections with vertical line"
    (let [c (c/circle (p/point 0  0) 1)
          l7 (l/line (p/point  4 -2) (p/point 4 2))
          i7 (c/intersect-line-segment c l7)]
      (is (= 0 (count i7))))))

(deftest boxing
  (let [circle (c/circle (p/point 0 0) 1)
        box (c/outer-box circle)]
    (= (p/point -1 -1) (:p1 box))
    (= (p/point -1  1) (:p2 box))
    (= (p/point  1 -1) (:p3 box))
    (= (p/point  1  1) (:p4 box))))
