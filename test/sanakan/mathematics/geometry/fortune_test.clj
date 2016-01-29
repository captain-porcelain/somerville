(ns sanakan.mathematics.geometry.fortune-test
  (:require
    [clojure.zip :as z]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.fortune :as f]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; check creation and sorting of events
(def point1 (p/point 2 2))
(def event1 (f/event point1 :site))
(fact (:p event1) => point1)

(def point2 (p/point 1 3))
(def point3 (p/point 1 1))

(def points1 (list point1 point2 point3))
(def events1 (f/events points1))

(fact (count events1) => 3)
(fact (:p (nth events1 0)) => point3)
(fact (:p (nth events1 1)) => point1)
(fact (:p (nth events1 2)) => point2)

(def voronoi1-step0 (f/voronoi points1))
(dorun (println (c/out voronoi1-step0)))
(def voronoi1-step1 (f/step voronoi1-step0))
(dorun (println (c/out voronoi1-step1)))
