(ns sanakan.mathematics.geometry.line-test
  (:require
    [clojure.math.numeric-tower :as nt]
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; verify that lines can be calculated from points.
(def l1 (l/line (p/point 0 0) (p/point 1 1)))

;; check that parameterized helpers work
(fact (l/parameter-by-x l1 0) => 0)
(fact (l/parameter-by-x l1 1) => 1)
(def l2 (l/line (p/point 0 0) (p/point 1 2)))
(fact (l/parameter-by-x l2 0) => 0)
(fact (l/parameter-by-x l2 1) => 1)

;; verify that a line can be used to calculate y values using the slope intercept form
(fact (l/solve-line-at-sloped l1 0) => 0)
(fact (l/solve-line-at-sloped l1 1) => 1)
(def il1 (l/line (p/point 0 0) (p/point 2 1)))
(fact (l/solve-line-at-sloped il1 0) => 0)
(fact (l/solve-line-at-sloped il1 1) => 1/2)
(fact (l/solve-line-at-sloped il1 2) => 1)
(def il2 (l/line (p/point 1 0) (p/point 2 1)))
(fact (l/solve-line-at-sloped il2 0) => -1)
(fact (l/solve-line-at-sloped il2 1) => 0)
(fact (l/solve-line-at-sloped il2 2) => 1)

;; verify that a parameterized line can be used to calculate y values
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
(def il3 (l/line (p/point 1 0) (p/point 1 1)))
(fact (l/solve-line-at il3 0) => nil)
(fact (l/solve-line-at il3 2) => nil)

;; test that a bisector line between two points can be calculated.
(def b1 (l/bisector (p/point 0 1) (p/point 1 0)))
(fact (:p1 b1) => (p/point 0 0))
(fact (:p2 b1) => (p/point 1 1))

(def b2 (l/bisector (p/point 0 2) (p/point 1 1)))
(fact (:p1 b2) => (p/point 0 1))
(fact (:p2 b2) => (p/point 1 2))

(def b3 (l/bisector (p/point 0 1) (p/point 0 3)))
(fact (:p1 b3) => (p/point 0 2))
(fact (:p2 b3) => (p/point 1 2))

(def b4 (l/bisector (p/point 1 0) (p/point 3 0)))
(fact (:p1 b4) => (p/point 2 0))
(fact (:p2 b4) => (p/point 2 1))

(def b5 (l/bisector (p/point 0 0) (p/point 0 0)))
(fact (:p1 b5) => nil)

;; test calculation of intersection between two lines.
(def i1 (l/intersect il1 il2))
(fact (:x i1) => 2)
(fact (:y i1) => 1)
(def i2 (l/intersect il3 il3))
(fact i2 => nil)
(def i3 (l/intersect (l/line (p/point 0 0) (p/point 0 1)) (l/line (p/point 0 0) (p/point 1 1))))
(fact (:x i3) => 0)
(fact (:y i3) => 0)
(def i4 (l/intersect (l/line (p/point 0 0) (p/point 1 1)) (l/line (p/point 0 0) (p/point 0 1))))
(fact (:x i4) => 0)
(fact (:y i4) => 0)
(def i5 (l/intersect (l/line (p/point 0 0) (p/point 1 0)) (l/line (p/point 0 0) (p/point 1 1))))
(fact (:x i5) => 0)
(fact (:y i5) => 0)
(def i6 (l/intersect (l/line (p/point 0 0) (p/point 1 1)) (l/line (p/point 0 0) (p/point 1 0))))
(fact (:x i6) => 0)
(fact (:y i6) => 0)
(def i8 (l/intersect (l/line (p/point 1 1) (p/point 2 3)) (l/line (p/point 0 2) (p/point 1 2))))
(fact (c/close-to (:x i8) 1.5) => true)
(fact (c/close-to (:y i8) 2.0) => true)

(def il3 (l/line (p/point 0 0) (p/point 0 1)))
(def il4 (l/line (p/point 2 0) (p/point 2 1)))
(fact (l/parallel? il3 il4) => true)
(def i2 (l/intersect il3 il4))
(fact i2 => nil)

(fact (l/parallel? (l/line (p/point 0 0) (p/point 0 1)) (l/line (p/point 0 0.5) (p/point 0 0.5))) => true)
(fact (l/parallel? (l/line (p/point 0 0.5) (p/point 1 0.5)) (l/line (p/point 0 0) (p/point 1 0))) => true)
(def i3 (l/intersect (l/line (p/point 1 1) (p/point 2 3)) (l/line (p/point 0 0.5) (p/point 1 0.5))))

(def cl1 (l/line (p/point 0 0) (p/point 0 1)))
(def cl2 (l/line (p/point 1 0) (p/point 1 1)))
(def cl3 (l/line (p/point 0 0) (p/point 1 0)))
(def cl4 (l/line (p/point 0 2) (p/point 1 2)))
(def c (l/cuts cl1 (list cl2 cl3 cl4)))
(fact (count c) => 2)
