(ns sanakan.mathematics.geometry.line-test
  (:require
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

;; very that lines can ba calculated from points.
(def l1 (l/line (p/point 0 0) (p/point 1 1)))
(fact (:a l1) => 1)
(fact (:b l1) => 0)
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

;; test that a bisector line between two points can be calculated.
(def b1 (l/bisector (p/point 0 1) (p/point 1 0)))
(fact (:a l1) => 1)
(fact (:b l1) => 0)

;; test calculation of intersection between two lines.
(def i1 (l/intersect il1 il2))
(fact (:x i1) => 2)
(fact (:y i1) => 1)
