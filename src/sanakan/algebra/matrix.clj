(ns sanakan.algebra.matrix
  (:use [sanakan.algebra.array]))

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
             (dorun (println (str i " " j " " w " " h)))
             (do (aset! mt j i (aget! m i j)))
             (recur (unchecked-inc j))))
         (recur (unchecked-inc i))))
    mt))

(defn decompose-lower
  "Calculate the matrix L used for LU decomposition."
  [m l n]
  (let [w (count m)
        ln (make-array Double/TYPE w w)]
     (loop [i (int 0)]
       (when (< i w)
         (loop [j (int 0)]
           (when (< j w)
             (dorun (println (str i " " j " " w)))
             (let [v (if (= i j 1.0 (if (and (> i j) (= j n)) (/ (aget! m i j) (aget! m j j)) 0.0)))]
               (do (aset! ln j i v))
               (do (when (and (> i j) (= j n)) (aset! l j i v))))
             (recur (unchecked-inc j))))
         (recur (unchecked-inc i))))
    {:ln ln :l l}))

(defn lu-decomposition
  "Decompose a square matrix into lower and upper matirces. Useful for solving linear equations, calculating the determinant and inversion."
  [m]
  )
