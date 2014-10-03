(ns sanakan.mathematics.geometry.line-test
  (:require [sanakan.mathematics.geometry.line :as l])
  (:use midje.sweet))

(def l1 (l/from-points (struct-map l/point2 :x 0 :y 0) (struct-map l/point2 :x 1 :y 1)))
(fact (:a l1) => 1)
(fact (:b l1) => 0)
