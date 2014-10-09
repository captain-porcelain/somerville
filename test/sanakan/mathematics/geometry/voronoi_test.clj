(ns sanakan.mathematics.geometry.voronoi-test
  (:require
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.voronoi :as v]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; test bisector helper functions
(def p1 (p/point 1 1))
(def p2 (p/point 3 7))
(def points1 (list p1 p2))
(def bisectors1 (v/bisectors points1))
(fact (count bisectors1) => 2)
(fact (count (:bisectors (first bisectors1))) => 1)
(fact (:point (first bisectors1)) => p1)
(fact (count (:bisectors (second bisectors1))) => 1)
(fact (:point (second bisectors1)) => p2)


;; test properties of calculated voronois
(def p3 (p/point 1 1))
(def p4 (p/point 4 2))
(def p5 (p/point 2 5))
(def p6 (p/point 6 7))
(def points2 (list p3 p4 p5 p6))
(def v1 (v/voronoi points2 0 0 10 10))

(fact (count (:intersections (v/intersect-bisectors (first (:points v1))))) => 6)

;; this logic only holds if there are no two bisectors that are parallel.
;; the voronoi contains the same amount of points as it was given.
(fact (count (:points v1)) => (count points2))
(dorun
  (for [p (:points v1)]
    ;; each point has as many bisectors as other points exist.
    (fact (count (:bisectors p)) => (- (count points2) 1))))
(dorun
  (for [p (:points v1)]
    ;; each bisector has one intersection with the other bisectors.
    (fact (count (:intersections p)) => (/ (* (count points2) (- (count points2) 1)) 2))))

;(dorun (println (c/out (first (:bisectors (first v1))))))
;(dorun (println (c/out (first (:intersections (first v1))))))
(dorun (println (c/out v1)))
