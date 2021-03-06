(ns somerville.data-structures.matrix-test
  (:use [somerville.data-structures.array])
  (:use [somerville.data-structures.matrix])
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

(deftest test-lu-decomposition
  (let [m (make-array Double/TYPE 2 2)
        r1 (aset! m 0 0 4.0)
        r2 (aset! m 0 1 3.0)
        r3 (aset! m 1 0 6.0)
        r4 (aset! m 1 1 3.0)
        decomposed (lu-decomposition m)
        l (:l decomposed)
        u (:u decomposed) ]
    (is (= 1.0 (aget! l 0 0)))
    (is (= 0.0 (aget! l 0 1)))
    (is (= 1.5 (aget! l 1 0)))
    (is (= 1.0 (aget! l 1 1)))
    (is (= 4.0 (aget! u 0 0)))
    (is (= 3.0 (aget! u 0 1)))
    (is (= 0.0 (aget! u 1 0)))
    (is (= -1.5 (aget! u 1 1)))))

(deftest test-multiplication
  (let [m (make-array Double/TYPE 4 3)
        r11 (aset! m 0 0 14.0)
        r12 (aset! m 0 1 9.0)
        r13 (aset! m 0 2 3.0)
        r21 (aset! m 1 0 2.0)
        r22 (aset! m 1 1 11.0)
        r22 (aset! m 1 2 15.0)
        r31 (aset! m 2 0 0.0)
        r32 (aset! m 2 1 12.0)
        r32 (aset! m 2 2 17.0)
        r41 (aset! m 3 0 5.0)
        r42 (aset! m 3 1 2.0)
        r42 (aset! m 3 2 3.0)
        n (make-array Double/TYPE 3 2)
        q11 (aset! n 0 0 12.0)
        q12 (aset! n 0 1 25.0)
        q21 (aset! n 1 0 9.0)
        q22 (aset! n 1 1 10.0)
        q31 (aset! n 2 0 8.0)
        q32 (aset! n 2 1 5.0)
        mn (multiply m n)]
    (is (= 273.0 (aget! mn 0 0)))
    (is (= 455.0 (aget! mn 0 1)))
    (is (= 243.0 (aget! mn 1 0)))
    (is (= 235.0 (aget! mn 1 1)))
    (is (= 244.0 (aget! mn 2 0)))
    (is (= 205.0 (aget! mn 2 1)))
    (is (= 102.0 (aget! mn 3 0)))
    (is (= 160.0 (aget! mn 3 1)))))

(deftest test-multiplication-2
  (let [m (make-array Double/TYPE 2 2)
        r11 (aset! m 0 0 1.0)
        r12 (aset! m 0 1 0.0)
        r13 (aset! m 1 0 -1.5)
        r21 (aset! m 1 1 1.0)
        n (make-array Double/TYPE 2 2)
        q11 (aset! n 0 0 4.0)
        q12 (aset! n 0 1 3.0)
        q21 (aset! n 1 0 6.0)
        q22 (aset! n 1 1 3.0)
        mn (multiply m n)]
    (is (= 4.0 (aget! mn 0 0)))
    (is (= 3.0 (aget! mn 0 1)))
    (is (= 0.0 (aget! mn 1 0)))
    (is (= -1.5 (aget! mn 1 1)))))

(deftest test-determinant
  (let [m (make-array Double/TYPE 2 2)
        r11 (aset! m 0 0 -2.0)
        r12 (aset! m 0 1 2.0)
        r21 (aset! m 1 0 -1.0)
        r22 (aset! m 1 1 1.0)
        d (determinant m)]
    (is (= 0.0 d))))

(deftest test-solve-lower-vector
  (let [m (make-array Double/TYPE 2 2)
        r11 (aset! m 0 0 1.0)
        r12 (aset! m 0 1 0.0)
        r13 (aset! m 1 0 1.0)
        r21 (aset! m 1 1 1.0)
        x (make-array Double/TYPE 2)
        q11 (aset! x 0 4.0)
        q12 (aset! x 1 6.0)
        y (solve-lower-array m x)]
    (is (= 4.0 (aget! y 0)))
    (is (= 2.0 (aget! y 1)))))

(deftest test-solve-upper-vector
  (let [m (make-array Double/TYPE 2 2)
        r11 (aset! m 0 0 2.0)
        r12 (aset! m 0 1 1.0)
        r13 (aset! m 1 0 0.0)
        r21 (aset! m 1 1 3.0)
        x (make-array Double/TYPE 2)
        q11 (aset! x 0 16.0)
        q12 (aset! x 1 24.0)
        y (solve-upper-array m x)]
    (is (= 4.0 (aget! y 0)))
    (is (= 8.0 (aget! y 1)))))

(deftest test-solve-vector
  (let [m (make-array Double/TYPE 2 2)
        r11 (aset! m 0 0 4.0)
        r12 (aset! m 0 1 2.0)
        r13 (aset! m 1 0 1.0)
        r21 (aset! m 1 1 3.0)
        x (make-array Double/TYPE 2)
        q11 (aset! x 0 34.0)
        q12 (aset! x 1 26.0)
        y (solve-array m x)]
    (is (= 5.0 (aget! y 0)))
    (is (= 7.0 (aget! y 1)))))

(deftest test-solve-lower-matrix
  (let [m (make-array Double/TYPE 2 2)
        r11 (aset! m 0 0 1.0)
        r12 (aset! m 0 1 0.0)
        r21 (aset! m 1 0 2.0)
        r22 (aset! m 1 1 1.0)
        x (make-array Double/TYPE 2 2)
        q11 (aset! x 0 0 3.0)
        q12 (aset! x 0 1 2.0)
        q21 (aset! x 1 0 10.0)
        q22 (aset! x 1 1 9.0)
        y (solve-lower-matrix m x)]
    (is (= 3.0 (aget! y 0 0)))
    (is (= 2.0 (aget! y 0 1)))
    (is (= 4.0 (aget! y 1 0)))
    (is (= 5.0 (aget! y 1 1)))))

(deftest test-solve-upper-matrix
  (let [m (make-array Double/TYPE 2 2)
        r11 (aset! m 0 0 1.0)
        r12 (aset! m 0 1 2.0)
        r21 (aset! m 1 0 0.0)
        r22 (aset! m 1 1 2.0)
        x (make-array Double/TYPE 2 2)
        q11 (aset! x 0 0 11.0)
        q12 (aset! x 0 1 12.0)
        q21 (aset! x 1 0 8.0)
        q22 (aset! x 1 1 10.0)
        y (solve-upper-matrix m x)]
    (is (= 3.0 (aget! y 0 0)))
    (is (= 2.0 (aget! y 0 1)))
    (is (= 4.0 (aget! y 1 0)))
    (is (= 5.0 (aget! y 1 1)))))

(deftest test-invert
  (let [m (make-array Double/TYPE 3 3)
        r11 (aset! m 0 0 1.0)
        r12 (aset! m 0 1 2.0)
        r13 (aset! m 0 2 0.0)
        r21 (aset! m 1 0 2.0)
        r22 (aset! m 1 1 3.0)
        r23 (aset! m 1 2 0.0)
        r31 (aset! m 2 0 3.0)
        r32 (aset! m 2 1 4.0)
        r33 (aset! m 2 2 1.0)
        y (invert m)]
    (is (= -3.0 (aget! y 0 0)))
    (is (= 2.0 (aget! y 0 1)))
    (is (= 0.0 (aget! y 0 2)))
    (is (= 2.0 (aget! y 1 0)))
    (is (= -1.0 (aget! y 1 1)))
    (is (= 0.0 (aget! y 1 2)))
    (is (= 1.0 (aget! y 2 0)))
    (is (= -2.0 (aget! y 2 1)))
    (is (= 1.0 (aget! y 2 2)))))
