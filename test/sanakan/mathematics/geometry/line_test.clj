(ns sanakan.mathematics.geometry.line-test
  (:require
    [sanakan.mathematics.geometry.point :as p]
    [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

(def l1 (l/line (p/point 0 0) (p/point 1 1)))
(fact (:a l1) => 1)
(fact (:b l1) => 0)

(def b1 (l/bisector (p/point 0 1) (p/point 1 0)))
(fact (:a l1) => 1)
(fact (:b l1) => 0)

