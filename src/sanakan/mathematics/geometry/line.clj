(ns sanakan.mathematics.geometry.line
  (:require
    [sanakan.mathematics.geometry.point :as p]))

;; define a line by having ax + b
(defstruct line2 :a :b :p1 :p2)

(defn line
  "Get a line from two points."
  [p1 p2]
  (let [s (p/slope p1 p2)]
    (struct-map line2 :a s :b (- (:y p1) (* s (:x p1))) :p1 p1 :p2 p2)))

(defn solve-line-at
  "A line is given by y = a*x + b. This function solves this for a given x."
  [line x]
  (+ (* (:a line) x) (:b line)))

(defn bisector
  "Get the line that bisects two points."
  [p1 p2]
  (let [s (* -1 (/ 1 (p/slope p1 p2)))
        m (p/midpoint p1 p2)
        l (line p1 p2)
        t (- (:y m) (* s (:a l)))
        b (struct-map line2 :a s :b t)
        y1 (solve-line-at b 0)
        y2 (solve-line-at b 1)]
    (struct-map line2 :a s :b t :p1 (p/point 0 y1) :p2 (p/point 1 y2))))

(defn intersect
  "Get intersection point of two lines."
  [l1 l2]
  (let [x (/ (- (:b l2) (:b l1)) (- (:a l1) (:a l2)))
        y (solve-line-at l1 x)]
    (p/point x y)))
