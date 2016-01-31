(ns sanakan.mathematics.geometry.commons)

(defn indent
  [i]
  (reduce str (repeat i " ")))

;; Define a protocol for pretty printing geometry records.
(defprotocol Printable
  (out [this] [this x] [this x k]))

(defn close-to
  "Compare floats."
  [x y]
  (and (< x (+ y 0.001)) (> x (- y 0.001))))
