(ns sanakan.mathematics.geometry.line)

;; define a two dimensional point
(defstruct point2 :x :y)

;; define a line by having two points
(defstruct line :a :b)

(defn slope
  [p1 p2]
  (let [dx (- (:x p2) (:x p1))
        dy (- (:y p2) (:y p1))]
    (if (= dx 0) 999999 (/ dy dx))))

(defn from-points
  [p1 p2]
  (struct-map line :a (slope p1 p2) :b (:y p1)))

(defn bisector
  [p1 p2]
  (let [s (/ 1 (slope p1 p2))]
        ))
