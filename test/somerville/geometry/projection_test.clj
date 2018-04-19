(ns somerville.geometry.projection-test
  (:require
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p]
    [somerville.geometry.plane :as pl]
    [somerville.geometry.projection :as projection]
    [somerville.geometry.commons :as c])
  (:use clojure.test))

(deftest setup-projection-plane
  (let [camera (p/point 0 0 10)
        focus  (p/point 0 0  0)
        up     (p/point 0 1  0)
        plane (projection/projection-plane camera focus up 10)]
    (is (p/close? (p/point  0 0   0) (:p1 plane)))
    (is (p/close? (p/point  0 1   0) (:p2 plane)))
    (is (p/close? (p/point 10 0   0) (:p3 plane)))
    (is (p/close? (p/point  0 0 -10) (:n plane)))))

(deftest basic-projection
  (let [camera (p/point 0 0 10)
        focus  (p/point 0 0  0)
        up     (p/point 0 1  0)
        width  10
        height 10
        projector (projection/projector camera focus up 10 width height)
        p3d1    (p/point 0 0 -1)
        p2d1    (projection/project projector p3d1)
        p3d2    (p/point 10 0 0)
        p2d2    (projection/project projector p3d2)
        ;tmp (dorun (println (c/out p2d2)))
        ]
    (is (p/close? (p/point 5.0 5.0 0.0) p2d1))))

