(ns sanakan.mathematics.matrix
  (:use [sanakan.mathematics.array]))

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
             (do (aset! mt j i (aget! m i j)))
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
                 (do (aset! mn i j (+ (aget! mn i j) (* (aget! m i k) (aget! n k j)))))
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
             (let [v (if (= i j) 1.0 (if (and (> i j) (= j n)) (* -1 (/ (aget! m i j) (aget! m j j))) 0.0))]
               (do (aset! ln i j v))
               (do (when (= j n)
                     (when (> i j) (aset! l i j (* -1 v)))
                     (when (= i j) (aset! l i j v)))))
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
         (recur (unchecked-inc i) (* det (aget! u i i)))))))
