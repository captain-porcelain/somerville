(ns sanakan.mathematics.color.color-test
  (:require
    [sanakan.mathematics.color.color :as c]
    [sanakan.mathematics.geometry.commons :as gc])
  (:use midje.sweet))

(def c1 (c/rgba -16777216))
(fact (:r c1) => 0)
(fact (:g c1) => 0)
(fact (:b c1) => 0)

(def c2 (c/rgba -1879576))
(fact (:r c2) => 232)
(fact (:g c2) => 82)
(fact (:b c2) => 228)

(def c1 (c/rgba 0 0 0))
(def c2 (c/rgba 1 1 1))
(def c3 (c/rgba 2 2 2))
(def c4 (c/rgba 10 10 10))
(fact (c/cie76 c1 c1) => 0.0)
(fact (c/cie76 c1 c2) => 1.0)
(fact (c/cie76 c1 c3) => 1.0)
(fact (gc/close-to (c/cie76 c1 c4) 3.741657) => true)
