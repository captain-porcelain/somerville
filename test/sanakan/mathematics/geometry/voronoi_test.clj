(ns sanakan.mathematics.geometry.voronoi-test
  (:require
    [sanakan.mathematics.geometry.voronoi :as v]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; test bisector helper functions
(def p1 (p/point 1 1))
(def p2 (p/point 3 7))
(def points1 (list p1 p2))
(def bisectors1 (v/calculate-all-bisectors points1))
(fact (count bisectors1) => 2)
(fact (count (:bisectors (first bisectors1))) => 1)
(fact (:p (first bisectors1)) => p1)
(fact (count (:bisectors (second bisectors1))) => 1)
(fact (:p (second bisectors1)) => p2)


;; test properties of calculated voronois
(def p3 (p/point 1 1))
(def p4 (p/point 4 2))
(def p5 (p/point 2 5))
(def p6 (p/point 6 7))
(def points2 (list p3 p4 p5 p6))
(def v1 (v/voronoi points2 0 0 10 10))

;; this logic only holds if there are no two bisectors that are parallel.
;; the voronoi contains the same amount of points as it was given.
(fact (count v1) => (count points2))
(dorun
  (for [p v1]
    (let [bisectors (:bisectors p)]
      ;; each point has as many bisectors as other points exist.
      (fact (count bisectors) => (- (count points2) 1))
      (dorun
        (for [bisector bisectors]
          ;; each bisector has one intersection with the other bisectors.
          (fact (count (:intersections bisector)) => (- (count points2) 2)))))))

(dorun (println v1))
