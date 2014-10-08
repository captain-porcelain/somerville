(ns sanakan.mathematics.geometry.line
  (:require
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.point :as p]))

;; Define a line by its slope-intercept form having ax + b
(defrecord Line2 [a b p1 p2]
  c/Printable
  (c/out [this i] (str (c/indent i) "Line from " (c/out p1 (+ i 2)) " to " (c/out p2 (+ i 2)) " is f(x)=" a "*x+" b))
  (c/out [this] (c/out this 0)))

(defn line
  "Get a line from two points."
  [p1 p2]
  (let [s (p/slope p1 p2)]
    (Line2. s (- (:y p1) (* s (:x p1))) p1 p2)))

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
        t (- (:y m) (* s (:x m)))
        b (Line2. s t 0 1)
        y1 (solve-line-at b 0)
        y2 (solve-line-at b 1)]
    (Line2. s t (p/point 0 y1) (p/point 1 y2))))

(defn intersect
  "Get intersection point of two lines."
  [l1 l2]
  (let [x (/ (- (:b l2) (:b l1)) (- (:a l1) (:a l2)))
        y (solve-line-at l1 x)]
    (p/point x y)))
