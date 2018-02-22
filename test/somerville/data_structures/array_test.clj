(ns somerville.data-structures.array-test
  (:require
    [somerville.data-structures.array :as a])
  (:use [clojure.test]))

(deftest test-transpose
  (let [a1 (a/reshape-to-2d-array '(0 1 2 3 4 5 6 7) 2 4)]
    (is (= (a/aget! a1 0 0) 0.0))
    (is (= (a/aget! a1 0 1) 1.0))
    (is (= (a/aget! a1 0 2) 2.0))
    (is (= (a/aget! a1 0 3) 3.0))
    (is (= (a/aget! a1 1 0) 4.0))
    (is (= (a/aget! a1 1 1) 5.0))
    (is (= (a/aget! a1 1 2) 6.0))
    (is (= (a/aget! a1 1 3) 7.0))))
