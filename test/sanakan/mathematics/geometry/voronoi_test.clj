(ns sanakan.mathematics.geometry.voronoi-test
  (:require
    [sanakan.mathematics.geometry.voronoi :as v]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

(def p1 (p/point 1 1))
(def p2 (p/point 3 7))
(def points1 (list p1 p2))
(def bisectors1 (v/calculate-all-bisectors points1))
(fact (count bisectors1) => 2)
(fact (count (:bisectors (first bisectors1))) => 1)
(fact (:p (first bisectors1)) => p1)
(fact (count (:bisectors (second bisectors1))) => 1)
(fact (:p (second bisectors1)) => p2)

(def p3 (p/point 1 1))
(def p4 (p/point 4 2))
(def p5 (p/point 2 5))
(def points2 (list p3 p4 p5))
(dorun (println (v/voronoi points2 0 0 10 10)))
