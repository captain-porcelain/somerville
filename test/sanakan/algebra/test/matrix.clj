(ns sanakan.algebra.test.matrix
  (:use [sanakan.algebra.array])
  (:use [sanakan.algebra.matrix])
  (:use [clojure.test]))

(deftest test-transpose
  (let [m (make-array Double/TYPE 2 2)
        r1 (aset! m 0 0 1.0)
        r2 (aset! m 0 1 2.0)
        r3 (aset! m 1 0 3.0)
        r4 (aset! m 1 1 4.0)
        mt (transpose m)]
    (is (= 1.0 (aget! mt 0 0)))
    (is (= 3.0 (aget! mt 0 1)))
    (is (= 2.0 (aget! mt 1 0)))
    (is (= 4.0 (aget! mt 1 1)))))

