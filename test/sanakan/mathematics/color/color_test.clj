(ns sanakan.mathematics.color.color-test
  (:require
    [sanakan.mathematics.color.color :as c])
  (:use midje.sweet))

(def c1 (c/rgba -16777216))
(fact (:r c1) => 0)
(fact (:g c1) => 0)
(fact (:b c1) => 0)

(def c2 (c/rgba -1879576))
(fact (:r c2) => 232)
(fact (:g c2) => 82)
(fact (:b c2) => 228)
