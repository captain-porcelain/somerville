(ns somerville.geometry.plane-test
  (:require
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p]
    [somerville.geometry.plane :as pl]
    [somerville.geometry.commons :as c])
  (:use clojure.test))

(deftest basics
  (let [line (l/line (p/point 0 0 10) (p/point 0 0 -10))
        plane (pl/plane (p/point -1 -1 0) (p/point -1 1 0) (p/point 1 -1 0))
        i (pl/intersect plane line)]
    (is (= (:x i) 0))
    (is (= (:y i) 0))
    (is (= (:z i) 0))))

