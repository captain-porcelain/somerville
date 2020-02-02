(ns somerville.geometry.commons)

(defn indent
  [i]
  (reduce str (repeat i " ")))

;; Define a protocol for pretty printing geometry records.
(defprotocol Printable
  (out [this] [this x] [this x k]))

(def epsilon 0.01)

(defn close-to
  "Compare floats."
  ([x y]
   (and (< x (+ y epsilon)) (> x (- y epsilon))))
  ([x y z]
   (and
     (< x (+ y epsilon))
     (> x (- y epsilon))
     (< x (+ z epsilon))
     (> x (- z epsilon)))))

(defn compareTo
  ".compareTo alternative to handle clojure BigInteger."
  [n1 n2]
  (cond
    (< n1 n2) -1
    (> n1 n2)  1
    :else      0))

(defn avg
  "calculate average of list of numbers"
  [numbers]
  (if (= 0 (count numbers))
    0
    (/ (reduce + numbers) (count numbers))))
