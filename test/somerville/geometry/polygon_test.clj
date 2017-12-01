(ns somerville.geometry.polygon-test
  (:require
    [somerville.geometry.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.polygon :as poly])
  (:use clojure.test))

(deftest polygon-creation
  (let [p1 (p/point -1 1)
        p2 (p/point 1 1)
        p3 (p/point 1 -1)
        p4 (p/point -1 -1)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points)]
    (is (= 4 (count (:lines polygon))))
    (is (= p1 (:p1 (nth (:lines polygon) 0))))
    (is (= p2 (:p2 (nth (:lines polygon) 0))))
    (is (= p2 (:p1 (nth (:lines polygon) 1))))
    (is (= p3 (:p2 (nth (:lines polygon) 1))))
    (is (= p3 (:p1 (nth (:lines polygon) 2))))
    (is (= p4 (:p2 (nth (:lines polygon) 2))))
    (is (= p4 (:p1 (nth (:lines polygon) 3))))
    (is (= p1 (:p2 (nth (:lines polygon) 3))))))

(deftest line-intersection
  (let [p1 (p/point -1 1)
        p2 (p/point 1 1)
        p3 (p/point 1 -1)
        p4 (p/point -1 -1)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points)
        line (l/line (p/point -2 0) (p/point 2 0))
        intersections (poly/intersect polygon line)]
  (is (= 2 (count intersections)))
  (is (= 1  (:x (nth intersections 0))))
  (is (= 0  (:y (nth intersections 0))))
  (is (= -1 (:x (nth intersections 1))))
  (is (= 0  (:y (nth intersections 1))))))

;; This is actually an edge case. Since the line goes through the center it is unclear which half should be used.
(deftest line-cut-simple
  (let [p1 (p/point -1 1)
        p2 (p/point 1 1)
        p3 (p/point 1 -1)
        p4 (p/point -1 -1)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points)
        line (l/line (p/point -2 0) (p/point 2 0))
        i1 (p/point 1 0)
        i2 (p/point -1 0)
        cut-result (poly/cut polygon line)]
    (is (= 4 (count (:lines cut-result))))
    (is (= p1 (:p1 (nth (:lines cut-result) 0))))
    (is (= p2 (:p2 (nth (:lines cut-result) 0))))
    (is (= p2 (:p1 (nth (:lines cut-result) 1))))
    (is (= i1 (:p2 (nth (:lines cut-result) 1))))
    (is (= i1 (:p1 (nth (:lines cut-result) 2))))
    (is (= i2 (:p2 (nth (:lines cut-result) 2))))
    (is (= i2 (:p1 (nth (:lines cut-result) 3))))
    (is (= p1 (:p2 (nth (:lines cut-result) 3))))))


;; This is actually an edge case. Since the line goes through the center it is unclear which half should be used.
(deftest line-cut-simple-2
  (let [p1 (p/point -1 1)
        p2 (p/point 1 1)
        p3 (p/point 1 -1)
        p4 (p/point -1 -1)
        points (list p1 p2 p3 p4)
        polygon (poly/from-points points)
        line (l/line (p/point 0 -2) (p/point 0 2))
        i1 (p/point 0 1)
        i2 (p/point 0 -1)
        cut-result (poly/cut polygon line)]
    (is (= 4 (count (:lines cut-result))))
    (is (= p1 (:p1 (nth (:lines cut-result) 0))))
    (is (= i1 (:p2 (nth (:lines cut-result) 0))))
    (is (= i1 (:p1 (nth (:lines cut-result) 1))))
    (is (= i2 (:p2 (nth (:lines cut-result) 1))))
    (is (= i2 (:p1 (nth (:lines cut-result) 2))))
    (is (= p4 (:p2 (nth (:lines cut-result) 2))))
    (is (= p4 (:p1 (nth (:lines cut-result) 3))))
    (is (= p1 (:p2 (nth (:lines cut-result) 3))))))



