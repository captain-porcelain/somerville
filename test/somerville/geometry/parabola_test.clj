(ns somerville.geometry.parabola-test
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.parabola :as par]
    [somerville.geometry.line :as l])
  (:use [clojure.test]))

;; test creation of parabolas in various forms
(deftest creation
  (let [parabola1 (par/parabola-from-factors 1 2 3)
        parabola2 (par/parabola-from-focuspoint-and-directrix (p/point 0 1) (l/line-from-slope 0 -1))]
    (is (= (:a parabola1) 1))
    (is (= (:b parabola1) 2))
    (is (= (:c parabola1) 3))
    (is (= (:a parabola2) 1/4))
    (is (= (:b parabola2) 0))
    (is (= (:c parabola2) 0))))

;; test calculation of discriminates
(deftest discriminates
  (is (= (par/discriminate (par/parabola-from-factors 2 0  0))  0))
  (is (= (par/discriminate (par/parabola-from-factors 2 0  1)) -1/2))
  (is (= (par/discriminate (par/parabola-from-factors 0 0 -1)) -1)))

;; test calculation of intersections
(deftest intersections
  (let [p1 (par/parabola-from-factors  2 0 0)
        p2 (par/parabola-from-factors 1 0 0)
        p3 (par/parabola-from-factors -2 0 1)
        p4 (par/parabola-from-factors 1 0 1)
        p5 (par/parabola-from-focuspoint-and-directrix (p/point 0 1) (l/line-from-slope 0 -1))
        p6 (par/parabola-from-focuspoint-and-directrix (p/point 1 1) (l/line-from-slope 0 -1))
        intersections1 (par/intersect-two-parabolas p1 p3)
        intersections2 (par/intersect-two-parabolas p2 p1)
        intersections3 (par/intersect-two-parabolas p5 p6)
        intersections4 (par/intersect-two-parabolas p2 p4)]
    (is (= (count intersections1) 2))
    (is (= (nth intersections1 0) (p/point -0.5 0.5)))
    (is (= (nth intersections1 1) (p/point  0.5 0.5)))

    (is (= (count intersections2) 1))
    (is (= (nth intersections2 0) (p/point 0.0 0.0)))

    (is (= (count intersections3) 1))
    (is (= (nth intersections3 0) (p/point 1/2 1/16)))

    (is (= (count intersections4) 0))))

;; test beachlines created by multiple parabolas
(deftest beachlines
  (let [beachline1 (par/beachline (list (par/parabola-from-factors 1 0 0) (par/parabola-from-factors 1 0 1)))
        beach-2-parabola-1 (par/parabola-from-focuspoint-and-directrix (p/point 0 1) (l/line-from-slope 0 -1))
        beach-2-parabola-2 (par/parabola-from-focuspoint-and-directrix (p/point 1 1) (l/line-from-slope 0 -1))
        beachline2 (par/beachline (list beach-2-parabola-1 beach-2-parabola-2))
        beach-3-parabola-1 (par/parabola-from-factors  2 0 0)
        beach-3-parabola-2 (par/parabola-from-factors -2 0 1)
        beachline3 (par/beachline (list beach-3-parabola-1 beach-3-parabola-2))
        beach-4-parabola-1 (par/parabola-from-focuspoint-and-directrix (p/point 500 300) (l/line-from-slope 0 280))
        beach-4-parabola-2 (par/parabola-from-focuspoint-and-directrix (p/point 400 400) (l/line-from-slope 0 280))
        beachline4 (par/beachline (list beach-4-parabola-1 beach-4-parabola-2))
        beach-5-parabola-1 (par/parabola-from-focuspoint-and-directrix (p/point -2 2) (l/line-from-slope 0 0))
        beach-5-parabola-2 (par/parabola-from-focuspoint-and-directrix (p/point  0 2) (l/line-from-slope 0 0))
        beach-5-parabola-3 (par/parabola-from-focuspoint-and-directrix (p/point  2 2) (l/line-from-slope 0 0))
        beachline5 (par/beachline (list beach-5-parabola-1 beach-5-parabola-2 beach-5-parabola-3))]
    (is (= (count (:intersections beachline1)) 0))
    (is (= (count (:parabolas beachline1)) 1))
    (is (= (first (:parabolas beachline1)) (par/parabola-from-factors 1 0 0)))
    (is (= (count (:intersections beachline2)) 1))
    (is (= (first (:intersections beachline2)) (p/point 1/2 1/16)))
    (is (= (count (:parabolas beachline2)) 2))
    (is (= (first (:parabolas beachline2)) beach-2-parabola-1))
    (is (= (second (:parabolas beachline2)) beach-2-parabola-2))
    (is (= (count (:intersections beachline3)) 2))
    (is (= (first (:intersections beachline3)) (p/point -0.5 0.5)))
    (is (= (second (:intersections beachline3)) (p/point 0.5 0.5)))
    (is (= (count (:parabolas beachline3)) 3))
    (is (= (nth (:parabolas beachline3) 0) beach-3-parabola-2))
    (is (= (nth (:parabolas beachline3) 1) beach-3-parabola-1))
    (is (= (nth (:parabolas beachline3) 2) beach-3-parabola-2))
    (is (= (par/get-parabola-from-beachline beachline3 -0.6) beach-3-parabola-2))
    (is (= (par/get-parabola-from-beachline beachline3 -0.4) beach-3-parabola-1))
    (is (= (par/get-parabola-from-beachline beachline3 0.4) beach-3-parabola-1))
    (is (= (par/get-parabola-from-beachline beachline3 0.6) beach-3-parabola-2))
    (is (= (count (:intersections beachline4)) 2))
    (is (= (par/get-parabola-from-beachline beachline4 (- (:x (nth (:intersections beachline4) 0)) 1)) beach-4-parabola-2))
    (is (= (par/get-parabola-from-beachline beachline4 (+ (:x (nth (:intersections beachline4) 0)) 1)) beach-4-parabola-1))
    (is (= (par/get-parabola-from-beachline beachline4 (- (:x (nth (:intersections beachline4) 1)) 1)) beach-4-parabola-1))
    (is (= (par/get-parabola-from-beachline beachline4 (+ (:x (nth (:intersections beachline4) 1)) 1)) beach-4-parabola-2))
    (is (= (count (:intersections beachline5)) 2))
    (is (= (par/get-parabola-from-beachline beachline5 -2) beach-5-parabola-1))
    (is (= (par/get-parabola-from-beachline beachline5  0) beach-5-parabola-2))
    (is (= (par/get-parabola-from-beachline beachline5  2) beach-5-parabola-3))))





