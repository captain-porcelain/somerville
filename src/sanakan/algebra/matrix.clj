(ns sanakan.algebra.matrix
  (:use [sanakan.algebra.array]))

(defn transpose
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
