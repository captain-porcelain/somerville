(ns sanakan.mathematics.test.array
  (:use [sanakan.mathematics.array])
  (:use [clojure.test]))

(deftest test-transpose
  (let [l '(0 1 2 3 4 5 6 7)
        a (reshape-to-2d-array l 2 4)]
    (is (= 0 (aget! a 0 0)))
    (is (= 1 (aget! a 0 1)))
    (is (= 2 (aget! a 0 2)))
    (is (= 3 (aget! a 0 3)))
    (is (= 4 (aget! a 1 0)))
    (is (= 5 (aget! a 1 1)))
    (is (= 6 (aget! a 1 2)))
    (is (= 7 (aget! a 1 3)))))
