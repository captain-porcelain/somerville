(ns somerville.color.color-test
  (:require
    [somerville.color.color :as c]
    [somerville.geometry.commons :as gc])
  (:use [clojure.test]))

(deftest color-conversion-int
  (let [c1 (c/rgba -16777216)
        c2 (c/rgba -1879576)]
    (is (= (:r c1) 0))
    (is (= (:g c1) 0))
    (is (= (:b c1) 0))
    (is (= (:r c2) 227))
    (is (= (:g c2) 81))
    (is (= (:b c2) 232))))

(deftest color-conversion-cie
  (let [c1 (c/rgba 0 0 0)
        c2 (c/rgba 1 1 1)
        c3 (c/rgba 2 2 2)
        c4 (c/rgba 10 10 10)]
    (is (= (c/cie76 c1 c1) 0.0))
    (is (= (c/cie76 c1 c2) 1.0))
    (is (= (c/cie76 c1 c3) 1.0))
    (is (= (gc/close-to (c/cie76 c1 c4) 3.741657) true))))
