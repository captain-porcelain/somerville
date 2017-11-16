(ns somerville.geometry.line-test
  (:require
    [clojure.math.numeric-tower :as nt]
    [somerville.geometry.point :as p]
    [somerville.geometry.commons :as c]
    [somerville.geometry.line :as l])
  (:use clojure.test))

;; verify that lines can be calculated from points.
(def l1 (l/line (p/point 0 0) (p/point 1 1)))
(def l2 (l/line (p/point 0 0) (p/point 1 2)))

;; check that parameterized helpers work
(deftest parameter-helpers
  (is (= (l/parameter-by-x l1 0) 0))
  (is (= (l/parameter-by-x l1 1) 1))
  (is (= (l/parameter-by-x l2 0) 0))
  (is (= (l/parameter-by-x l2 1) 1)))

;; verify that a line can be used to calculate y values using the slope intercept form
(def il1 (l/line (p/point 0 0) (p/point 2 1)))
(def il2 (l/line (p/point 1 0) (p/point 2 1)))

(deftest sloped
  (is (= (l/solve-line-at-sloped l1 0) 0))
  (is (= (l/solve-line-at-sloped l1 1) 1))
  (is (= (l/solve-line-at-sloped il1 0) 0))
  (is (= (l/solve-line-at-sloped il1 1) 1/2))
  (is (= (l/solve-line-at-sloped il1 2) 1))
  (is (= (l/solve-line-at-sloped il2 0) -1))
  (is (= (l/solve-line-at-sloped il2 1) 0))
  (is (= (l/solve-line-at-sloped il2 2) 1)))

;; verify that a parameterized line can be used to calculate y values
(def il1 (l/line (p/point 0 0) (p/point 2 1)))
(def il2 (l/line (p/point 1 0) (p/point 2 1)))
(def il3 (l/line (p/point 1 0) (p/point 1 1)))
(deftest parameterized
  (is (= (l/solve-line-at l1 0) 0))
  (is (= (l/solve-line-at l1 1) 1))
  (is (= (l/solve-line-at il1 0) 0))
  (is (= (l/solve-line-at il1 1) 1/2))
  (is (= (l/solve-line-at il1 2) 1))
  (is (= (l/solve-line-at il2 0) -1))
  (is (= (l/solve-line-at il2 1) 0))
  (is (= (l/solve-line-at il2 2) 1))
  (is (= (l/solve-line-at il3 0) nil))
  (is (= (l/solve-line-at il3 2) nil)))

;; test that a bisector line between two points can be calculated.
(def b1 (l/bisector (p/point 0 1) (p/point 1 0)))
(def b2 (l/bisector (p/point 0 2) (p/point 1 1)))
(def b3 (l/bisector (p/point 0 1) (p/point 0 3)))
(def b4 (l/bisector (p/point 1 0) (p/point 3 0)))
(def b5 (l/bisector (p/point 0 0) (p/point 0 0)))
(deftest bisectors
  (is (= (:p1 b1) (p/point 0 0)))
  (is (= (:p2 b1) (p/point 1 1)))

  (is (= (:p1 b2) (p/point 0 1)))
  (is (= (:p2 b2) (p/point 1 2)))

  (is (= (:p1 b3) (p/point 0 2)))
  (is (= (:p2 b3) (p/point 1 2)))

  (is (= (:p1 b4) (p/point 2 0)))
  (is (= (:p2 b4) (p/point 2 1)))

  (is (= (:p1 b5) nil)))

;; test calculation of intersection between two lines.
(def i1 (l/intersect il1 il2))
(def i2 (l/intersect il3 il3))
(def i3 (l/intersect (l/line (p/point 0 0) (p/point 0 1)) (l/line (p/point 0 0) (p/point 1 1))))
(def i4 (l/intersect (l/line (p/point 0 0) (p/point 1 1)) (l/line (p/point 0 0) (p/point 0 1))))
(def i5 (l/intersect (l/line (p/point 0 0) (p/point 1 0)) (l/line (p/point 0 0) (p/point 1 1))))
(def i6 (l/intersect (l/line (p/point 0 0) (p/point 1 1)) (l/line (p/point 0 0) (p/point 1 0))))
(def i8 (l/intersect (l/line (p/point 1 1) (p/point 2 3)) (l/line (p/point 0 2) (p/point 1 2))))
(deftest intersections
  (is (= (:x i1) 2))
  (is (= (:y i1) 1))
  (is (= i2 nil))
  (is (= (int (:x i3)) 0)) ;; TODO check if this is really a good idea
  (is (= (int (:y i3)) 0)) ;; TODO check if this is really a good idea
  (is (= (:x i4) 0))
  (is (= (:y i4) 0))
  (is (= (:x i5) 0))
  (is (= (:y i5) 0))
  (is (= (:x i6) 0))
  (is (= (:y i6) 0))
  (is (= (c/close-to (:x i8) 1.5) true))
  (is (= (c/close-to (:y i8) 2.0) true)))

(def il3 (l/line (p/point 0 0) (p/point 0 1)))
(def il4 (l/line (p/point 2 0) (p/point 2 1)))
(def i2 (l/intersect il3 il4))
(def i3 (l/intersect (l/line (p/point 1 1) (p/point 2 3)) (l/line (p/point 0 0.5) (p/point 1 0.5))))
(deftest parallels
  (is (= (l/parallel? il3 il4) true))
  (is (= i2 nil))

  (is (= (l/parallel? (l/line (p/point 0 0) (p/point 0 1)) (l/line (p/point 0 0.5) (p/point 0 0.5))) true))
  (is (= (l/parallel? (l/line (p/point 0 0.5) (p/point 1 0.5)) (l/line (p/point 0 0) (p/point 1 0))) true)))

(def plt1 (l/line (p/point 0 0) (p/point 1 0)))
(def n1 (l/normal plt1))
(def n2 (l/normal2 plt1))
(def pl1 (l/parallel plt1 1))
(deftest normals
  (is (= (c/close-to (p/distance (:p1 n1) (p/point 0.0 0.0)) 0) true))
  (is (= (c/close-to (p/distance (:p2 n1) (p/point 0.0 1.0)) 0) true))
  (is (= (c/close-to (p/distance (:p1 n2) (p/point 1.0 0.0)) 0) true))
  (is (= (c/close-to (p/distance (:p2 n2) (p/point 1.0 1.0)) 0) true))
  (is (= (c/close-to (p/distance (:p1 pl1) (p/point 0.0 1.0)) 0) true))
  (is (= (c/close-to (p/distance (:p2 pl1) (p/point 1.0 1.0)) 0) true)))

(def cl1 (l/line (p/point 0 0) (p/point 0 1)))
(def cl2 (l/line (p/point 1 0) (p/point 1 1)))
(def cl3 (l/line (p/point 0 0) (p/point 1 0)))
(def cl4 (l/line (p/point 0 2) (p/point 1 2)))
(def c (l/cuts cl1 (list cl2 cl3 cl4)))
(deftest cuts
  (is (= (count c) 2)))

(deftest segment-test
  (let [l1 (l/line (p/point -2 0) (p/point 2 0))
        p1 (p/point -2 0)
        p2 (p/point -2 1)]
    (is (= (l/point-on-segment? l1 p1) true))
    (is (= (l/point-on-segment? l1 p2) false))))
