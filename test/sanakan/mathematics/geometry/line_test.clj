(ns sanakan.mathematics.geometry.line-test
  (:require
    [clojure.math.numeric-tower :as nt]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; verify that lines can be calculated from points.
(def l1 (l/line (p/point 0 0) (p/point 1 1)))
(fact (:a l1) => 1)
(fact (:b l1) => 0)

;; check that parameterized helpers work
(fact (l/parameter-by-x l1 0) => 0)
(fact (l/parameter-by-x l1 1) => 1)
(def l2 (l/line (p/point 0 0) (p/point 1 2)))
(fact (l/parameter-by-x l2 0) => 0)
(fact (l/parameter-by-x l2 1) => 1)

;; verify that a line can be used to calculate y values
(fact (l/solve-line-at l1 0) => 0)
(fact (l/solve-line-at l1 1) => 1)
(def il1 (l/line (p/point 0 0) (p/point 2 1)))
(fact (l/solve-line-at il1 0) => 0)
(fact (l/solve-line-at il1 1) => 1/2)
(fact (l/solve-line-at il1 2) => 1)
(def il2 (l/line (p/point 1 0) (p/point 2 1)))
(fact (l/solve-line-at il2 0) => -1)
(fact (l/solve-line-at il2 1) => 0)
(fact (l/solve-line-at il2 2) => 1)

;; verify that a parameterized line can be used to calculate y values
(fact (l/solve-line-at-parameterized l1 0) => 0)
(fact (l/solve-line-at-parameterized l1 1) => 1)
(def il1 (l/line (p/point 0 0) (p/point 2 1)))
(fact (l/solve-line-at-parameterized il1 0) => 0)
(fact (l/solve-line-at-parameterized il1 1) => 1/2)
(fact (l/solve-line-at-parameterized il1 2) => 1)
(def il2 (l/line (p/point 1 0) (p/point 2 1)))
(fact (l/solve-line-at-parameterized il2 0) => -1)
(fact (l/solve-line-at-parameterized il2 1) => 0)
(fact (l/solve-line-at-parameterized il2 2) => 1)

;; test that a bisector line between two points can be calculated.
(def b1 (l/bisector (p/point 0 1) (p/point 1 0)))
(fact (:a b1) => 1)
(fact (:b b1) => 0)

(def b2 (l/bisector (p/point 0 2) (p/point 1 1)))
(fact (:a b2) => 1)
(fact (:b b2) => 1)

;; test calculation of intersection between two lines.
(def i1 (l/intersect il1 il2))
(fact (:x i1) => 2)
(fact (:y i1) => 1)

(def il3 (l/line (p/point 0 0) (p/point 0 1)))
(def il4 (l/line (p/point 2 0) (p/point 2 1)))
(def i2 (l/intersect il3 il4))
(fact i2 => nil)

(def cl1 (l/line (p/point 0 0) (p/point 0 1)))
(def cl2 (l/line (p/point 1 0) (p/point 1 1)))
(def cl3 (l/line (p/point 0 0) (p/point 1 0)))
(def cl4 (l/line (p/point 0 2) (p/point 1 2)))
(def c (l/cuts cl1 (list cl2 cl3 cl4)))
(fact (count c) => 2)
