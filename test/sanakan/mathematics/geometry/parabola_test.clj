(ns sanakan.mathematics.geometry.parabola-test
  (:require
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.parabola :as par]
    [sanakan.mathematics.geometry.line :as l])
  (:use [clojure.test])
  (:use midje.sweet))

;; test creation of parabolas in various forms
(def parabola1 (par/parabola-from-factors 1 2 3))
(fact (:a parabola1) => 1)
(fact (:b parabola1) => 2)
(fact (:c parabola1) => 3)

(def parabola2 (par/parabola-from-focuspoint-and-directrix (p/point 0 1) (l/line-from-slope 0 -1)))
(fact (:a parabola2) => 1/4)
(fact (:b parabola2) => 0)
(fact (:c parabola2) => 0)

;; test calculation of discriminates
(fact (par/discriminate (par/parabola-from-factors 2 0  0)) =>  0)
(fact (par/discriminate (par/parabola-from-factors 2 0  1)) => -1/2)
(fact (par/discriminate (par/parabola-from-factors 0 0 -1)) => -1)

;; test calculation of intersections
(def intersections1
  (par/intersect-two-parabolas
    (par/parabola-from-factors  2 0 0)
    (par/parabola-from-factors -2 0 1)))
(fact (count intersections1) => 2)
(fact (nth intersections1 0) => (p/point -0.5 0.5))
(fact (nth intersections1 1) => (p/point  0.5 0.5))

(def intersections2
  (par/intersect-two-parabolas
    (par/parabola-from-factors 1 0 0)
    (par/parabola-from-factors 2 0 0)))
(fact (count intersections2) => 1)
(fact (nth intersections2 0) => (p/point 0.0 0.0))

(def intersections3
  (par/intersect-two-parabolas
    (par/parabola-from-focuspoint-and-directrix (p/point 0 1) (l/line-from-slope 0 -1))
    (par/parabola-from-focuspoint-and-directrix (p/point 1 1) (l/line-from-slope 0 -1))))
(fact (count intersections3) => 1)
(fact (nth intersections3 0) => (p/point 1/2 1/16))

(def intersections4
  (par/intersect-two-parabolas
    (par/parabola-from-factors 1 0 0)
    (par/parabola-from-factors 1 0 1)))
(fact (count intersections4) => 0)

;; test beachlines created by multiple parabolas
(def beachline1 (par/beachline (list (par/parabola-from-factors 1 0 0) (par/parabola-from-factors 1 0 1))))
(fact (count (:intersections beachline1)) => 0)
(fact (count (:parabolas beachline1)) => 1)
(fact (first (:parabolas beachline1)) => (par/parabola-from-factors 1 0 0))

(def beach-2-parabola-1 (par/parabola-from-focuspoint-and-directrix (p/point 0 1) (l/line-from-slope 0 -1)))
(def beach-2-parabola-2 (par/parabola-from-focuspoint-and-directrix (p/point 1 1) (l/line-from-slope 0 -1)))
(def beachline2 (par/beachline (list beach-2-parabola-1 beach-2-parabola-2)))
(fact (count (:intersections beachline2)) => 1)
(fact (first (:intersections beachline2)) => (p/point 1/2 1/16))
(fact (count (:parabolas beachline2)) => 2)
(fact (first (:parabolas beachline2)) => beach-2-parabola-1)
(fact (second (:parabolas beachline2)) => beach-2-parabola-2)

(def beach-3-parabola-1 (par/parabola-from-factors  2 0 0))
(def beach-3-parabola-2 (par/parabola-from-factors -2 0 1))
(def beachline3 (par/beachline (list beach-3-parabola-1 beach-3-parabola-2)))
(fact (count (:intersections beachline3)) => 2)
(fact (first (:intersections beachline3)) => (p/point -0.5 0.5))
(fact (second (:intersections beachline3)) => (p/point 0.5 0.5))
(fact (count (:parabolas beachline3)) => 3)
(fact (nth (:parabolas beachline3) 0) => beach-3-parabola-2)
(fact (nth (:parabolas beachline3) 1) => beach-3-parabola-1)
(fact (nth (:parabolas beachline3) 2) => beach-3-parabola-2)
(fact (par/get-parabola-from-beachline beachline3 -0.6) => beach-3-parabola-2)
(fact (par/get-parabola-from-beachline beachline3 -0.4) => beach-3-parabola-1)
(fact (par/get-parabola-from-beachline beachline3 0.4) => beach-3-parabola-1)
(fact (par/get-parabola-from-beachline beachline3 0.6) => beach-3-parabola-2)


(def beach-4-parabola-1 (par/parabola-from-focuspoint-and-directrix (p/point 500 300) (l/line-from-slope 0 280)))
(def beach-4-parabola-2 (par/parabola-from-focuspoint-and-directrix (p/point 400 400) (l/line-from-slope 0 280)))
(def beachline4 (par/beachline (list beach-4-parabola-1 beach-4-parabola-2)))
(fact (count (:intersections beachline4)) => 2)
(fact (par/get-parabola-from-beachline beachline4 (- (:x (nth (:intersections beachline4) 0)) 1)) => beach-4-parabola-2)
(fact (par/get-parabola-from-beachline beachline4 (+ (:x (nth (:intersections beachline4) 0)) 1)) => beach-4-parabola-1)
(fact (par/get-parabola-from-beachline beachline4 (- (:x (nth (:intersections beachline4) 1)) 1)) => beach-4-parabola-1)
(fact (par/get-parabola-from-beachline beachline4 (+ (:x (nth (:intersections beachline4) 1)) 1)) => beach-4-parabola-2)

(def beach-5-parabola-1 (par/parabola-from-focuspoint-and-directrix (p/point -2 2) (l/line-from-slope 0 0)))
(def beach-5-parabola-2 (par/parabola-from-focuspoint-and-directrix (p/point  0 2) (l/line-from-slope 0 0)))
(def beach-5-parabola-3 (par/parabola-from-focuspoint-and-directrix (p/point  2 2) (l/line-from-slope 0 0)))
(def beachline5 (par/beachline (list beach-5-parabola-1 beach-5-parabola-2 beach-5-parabola-3)))
(fact (count (:intersections beachline5)) => 2)
(fact (par/get-parabola-from-beachline beachline5 -2) => beach-5-parabola-1)
(fact (par/get-parabola-from-beachline beachline5  0) => beach-5-parabola-2)
(fact (par/get-parabola-from-beachline beachline5  2) => beach-5-parabola-3)
