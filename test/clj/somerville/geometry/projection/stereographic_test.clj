(ns somerville.geometry.projection.stereographic-test
  (:require
    [somerville.geometry.line :as line]
    [somerville.geometry.point :as point]
    [somerville.geometry.sphere :as sphere]
    [somerville.geometry.projection.stereographic :as stereographic]
    [somerville.geometry.commons :as commons])
  (:use clojure.test))

(deftest basic-projection
  (let [sp1 (point/point 0 0 -1)
        pp1 (point/point 0 0)]
    (is (point/close? pp1 (stereographic/to-plane sp1)))
    (is (point/close? sp1 (stereographic/to-sphere pp1)))))

(deftest sphere-projection
  (let [sp (sphere/fibonacci 100)
        pp (map stereographic/to-plane sp)
        rp (map stereographic/to-sphere pp)]
    (map #(is (point/close? %1 %2)) sp rp)))
