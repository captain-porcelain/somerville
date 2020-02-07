;; An implementation of the stereographic projection as described on
;; https://en.wikipedia.org/wiki/Stereographic_projection
;; Projects points on a unit sphere onto a plane and back

(ns somerville.geometry.projection.stereographic
  (:require
    [somerville.geometry.line :as line]
    [somerville.geometry.point :as point]
    [somerville.geometry.plane :as plane]
    [somerville.geometry.commons :as commons]))

(defn to-plane
  "Project one point on a unit sphere onto a plane."
  [p]
  (point/point (/ (:x p) (- 1 (:z p))) (/ (:y p) (- 1 (:z p)))))

(defn to-sphere
  "Project a point from the plane onto a unit sphere."
  [p]
  (let [lower (+ 1 (* (:x p) (:x p)) (* (:y p) (:y p)))]
    (point/point
      (/ (* 2 (:x p)) lower)
      (/ (* 2 (:y p)) lower)
      (/ (+ -1 (* (:x p) (:x p)) (* (:y p) (:y p))) lower))))
