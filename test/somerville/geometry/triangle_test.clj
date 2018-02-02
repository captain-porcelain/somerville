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
    (is (= 1 h))))
