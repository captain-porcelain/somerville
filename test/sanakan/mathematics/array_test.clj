(ns sanakan.mathematics.array-test
  (:require
    [sanakan.mathematics.array :as a])
  (:use midje.sweet))

(def a1 (a/reshape-to-2d-array '(0 1 2 3 4 5 6 7) 2 4))
(fact (a/aget! a1 0 0) => 0.0)
(fact (a/aget! a1 0 1) => 1.0)
(fact (a/aget! a1 0 2) => 2.0)
(fact (a/aget! a1 0 3) => 3.0)
(fact (a/aget! a1 1 0) => 4.0)
(fact (a/aget! a1 1 1) => 5.0)
(fact (a/aget! a1 1 2) => 6.0)
(fact (a/aget! a1 1 3) => 7.0)
