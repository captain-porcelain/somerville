(ns somerville.matrix
  (:require [somerville.array :as a]))

(defn transpose
  "Transpose a matrix m. Put simply: make rows to columns. A 2x3 matrix will be a 3x2 matrix when transposed."
  [m]
  (let [w (count m)
        h (count (first m))
        mt (make-array Double/TYPE h w)]
     (loop [i (int 0)]
       (when (< i w)
         (loop [j (int 0)]
           (when (< j h)
             (do (a/aset! mt j i (a/aget! m i j)))
             (recur (unchecked-inc j))))
         (recur (unchecked-inc i))))
    mt))

(defn multiply
  "Multiply two matrices. Please note that if the first matix is 3x2 the second has to be 2x3."
  [m n]
  (let [r1 (count m)
        r2 (count (first n))
        r3 (count n)
        mn (make-array Double/TYPE r1 r2)]
     (loop [i (int 0)]
       (when (< i r1)
         (loop [j (int 0)]
           (when (< j r2)
             (loop [k (int 0)]
               (when (< k r3)
                 (do (a/aset! mn i j (+ (a/aget! mn i j) (* (a/aget! m i k) (a/aget! n k j)))))
                 (recur (unchecked-inc k))))
             (recur (unchecked-inc j))))
         (recur (unchecked-inc i))))
  mn))

(defn decompose-lower
  "Calculate the matrix L used for LU decomposition. m is the matrix to decompose in this iteration step,
  l is the matrix in which the lower values are collected and n is the current step."
  [m l n]
  (let [w (count m)
        ln (make-array Double/TYPE w w)]
     (loop [i (int 0)]
       (when (< i w)
         (loop [j (int 0)]
           (when (< j w)
             (let [v (if (= i j) 1.0 (if (and (> i j) (= j n)) (* -1 (/ (a/aget! m i j) (a/aget! m j j))) 0.0))]
               (do (a/aset! ln i j v))
               (do (when (= j n)
                     (when (> i j) (a/aset! l i j (* -1 v)))
                     (when (= i j) (a/aset! l i j v)))))
             (recur (unchecked-inc j))))
         (recur (unchecked-inc i))))
    ln))

(defn lu-decomposition
  "Decompose a square matrix into lower and upper matrices.
  Useful for solving linear equations, calculating the determinant and inversion."
  [m]
  (let [w (count m)
        l (make-array Double/TYPE w w)]
     (loop [i (int 0)
            a m]
       (let [ln (decompose-lower a l i)
             an (multiply ln a)]
         (if (>= i w)
           {:l l :u a}
           (recur (unchecked-inc i) an))))))

(defn determinant
  "Calculate the determinant of a square matrix."
  [m]
  (let [u (:u (lu-decomposition m))
        n (count u)]
     (loop [i (int 0)
            det 1.0]
       (if (>= i n)
         det
         (recur (unchecked-inc i) (* det (a/aget! u i i)))))))

(defn solve-lower-array
  "Solve the equation Ly = x where L is a lower triangular matrix and x is a vector."
  [l x]
  (let [n (count l)
        y (make-array Double/TYPE n)]
    (loop [i (int 0)]
      (when (< i n)
        (loop [j (int 0)
               sum (double 0.0)]
          (when (<= j i)
            (when (= j i)
              (a/aset! y i (/ (- (a/aget! x i) sum) (a/aget! l i j))))
            (recur (unchecked-inc j) (+ sum (* (a/aget! l i j) (a/aget! y j))))))
        (recur (unchecked-inc i))))
    y))

(defn solve-upper-array
  "Solve the equation Uy = x where U is a upper triangular matrix and x is a vector."
  [l x]
  (let [n (count l)
        y (make-array Double/TYPE n)]
    (loop [i (- n 1)]
      (when (>= i 0)
        (loop [j (- n 1)
               sum (double 0.0)]
          (when (>= j i)
            (when (= j i)
              (a/aset! y i (/ (- (a/aget! x i) sum) (a/aget! l i j))))
            (recur (unchecked-dec j) (+ sum (* (a/aget! l i j) (a/aget! y j))))))
        (recur (unchecked-dec i))))
    y))

(defn solve-array
  "Solve the linear equation My=x."
  [m x]
  (let [lu (lu-decomposition m)
        b (solve-lower-array (:l lu) x)
        y (solve-upper-array (:u lu) b)]
    y))

(defn solve-lower-matrix
  "Solve the equation Ly = x where L is a lower triangular matrix and x is a matrix."
  [l x]
  (let [n (count l)
        y (make-array Double/TYPE n n)]
    (loop [k (int 0)]
        (when (< k n)
          (loop [i (int 0)]
            (when (< i n)
              (loop [j (int 0)
                     sum (double 0.0)]
                (when (<= j i)
                  (when (= j i)
                    (a/aset! y i k (/ (- (a/aget! x i k) sum) (a/aget! l i j))))
                  (recur (unchecked-inc j) (+ sum (* (a/aget! l i j) (a/aget! y j k))))))
              (recur (unchecked-inc i))))
      (recur (unchecked-inc k))))
    y))

(defn solve-upper-matrix
  "Solve the equation Uy = x where U is a upperr triangular matrix and x is a matrix."
  [l x]
  (let [n (count l)
        y (make-array Double/TYPE n n)]
    (loop [k (int 0)]
        (when (< k n)
          (loop [i (- n 1)]
            (when (>= i 0)
            (loop [j (- n 1)
                     sum (double 0.0)]
                (when (>= j i)
                  (when (= j i)
                    (a/aset! y i k (/ (- (a/aget! x i k) sum) (a/aget! l i j))))
                  (recur (unchecked-dec j) (+ sum (* (a/aget! l i j) (a/aget! y j k))))))
              (recur (unchecked-dec i))))
      (recur (unchecked-inc k))))
    y))

(defn solve-matrix
  "Solve the linear equation My=x."
  [m x]
  (let [lu (lu-decomposition m)
        b (solve-lower-matrix (:l lu) x)
        y (solve-upper-matrix (:u lu) b)]
    y))

(defn identity-matrix
  "Get the identity matrix of size n"
  [n]
  (let [m (make-array Double/TYPE n n)]
    (loop [i (int 0)]
      (when (< i n)
        (loop [j (int 0)]
          (when (< j n)
            (if (= i j)
              (a/aset! m i j (double 1.0))
              (a/aset! m i j (double 0.0)))
            (recur (unchecked-inc j))))
        (recur (unchecked-inc i))))
    m))

(defn invert
  "Invert a square matrix."
  [m]
  (solve-matrix m (identity-matrix (count m))))
