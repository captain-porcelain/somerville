(ns sanakan.mathematics.geometry.line
  (:require
    [sanakan.mathematics.geometry.point :as p]))

;; define a line by having ax + b
(defstruct line2 :a :b)

(defn line
  "Get a line from two points."
  [p1 p2]
  (struct-map line2 :a (p/slope p1 p2) :b (:y p1)))

(defn bisector
  [p1 p2]
  (let [s (* -1 (/ 1 (p/slope p1 p2)))
        m (p/midpoint p1 p2)
        l (line p1 p2)
        b (- (:y m) (* s (:a l)))]
    (struct-map line2 :a s :b b)))
