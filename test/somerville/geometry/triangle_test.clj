(ns somerville.geometry.triangle-test
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.triangle :as t]
    [somerville.geometry.commons :as c])
  (:use clojure.test))

(deftest area
  (let [p1 (p/point 1 1)
        p2 (p/point 1 2)
        p3 (p/point 2 1)
        tr (t/triangle p1 p2 p3)
        a (t/area tr)]
    (is (= 1/2 a))))

(deftest height
  (let [p1 (p/point 1 1)
        p2 (p/point 1 2)
        p3 (p/point 2 1)
        tr (t/triangle p1 p2 p3)
        h (t/height tr)]
    (is (= 1.0 h))))

(deftest circumcircle
  (let [p1 (p/point 1 1)
        p2 (p/point 1 2)
        p3 (p/point 2 1)
        cp (p/point 3/2 3/2)

        tr (t/triangle p1 p2 p3)
        ccp (t/circumcenter tr)
        ccc (t/circumcircle tr)]
    (is (= cp ccp))
    (is (= cp (:p ccc)))
    (is (c/close-to 0.7071 (:r ccc)))))

(deftest inside
  (let [p1 (p/point 0 0)
        p2 (p/point 1 0)
        p3 (p/point 0 1)
        tr (t/triangle p1 p2 p3)
        p4 (p/point 1 1)
        p5 (p/point 0.25 0.25)]
    (is (false? (t/inside? tr p4)))
    (is (true? (t/inside? tr p5)))))
