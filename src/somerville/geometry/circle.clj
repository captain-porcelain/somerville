(ns somerville.geometry.circle
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.line :as l]
    [somerville.geometry.rectangle :as r]
    [somerville.geometry.point :as p]))

;; Define a circle from its center and radius.
(defrecord Circle2 [p r]
  c/Printable
  (c/out [this i] (str (c/indent i) "Circle at " (c/out p) " with radius " r))
  (c/out [this] (c/out this 0)))

(defn circle
  "Create a circle from a point and a radius."
  [p r]
  (Circle2. p r))

;; Given a circle as (x − p)² + (y − q)² = r² and a line as y = a*x + b"
;; we can insert by y so we get (x − p)² + (a*x + b − q)² = r²
;; This leads to (a² + 1) * x² + 2 * (a*b − a*q − p) * x + (q² − r² + p² − 2*b*q + b²) = 0
;; which we can solve like any regular quadratic function with (-B +- sqrt(B² - 4*A*C)) / 2*A
;; B² - 4*A*C indicates how many intersections exist. < 0 => none, = 1 => one, > 0 => two.
(defn intersect-non-vertical-line
  "Intersect circle and non vertical line."
  [circle line]
  (let [sl (l/slope-intercept line)
        A (+ (* (:a sl) (:a sl)) 1)
        B (* 2 (- (* (:a sl) (:b sl)) (* (:a sl) (:y (:p circle))) (:x (:p circle))))
        C (+ (- (* (:y (:p circle)) (:y (:p circle))) (* (:r circle) (:r circle)) (* 2 (:b sl) (:y (:p circle)))) (* (:x (:p circle)) (:x (:p circle))) (* (:b sl) (:b sl)))
        indicator (- (* B B) (* 4 A C))]
    (cond
      (< indicator 0) (list)
      (= indicator 0) (list (l/point-at line (/ (* -1 B) (* 2 A))))
      (> indicator 0) (list
                        (l/point-at line (/ (+ (* -1 B) (Math/sqrt indicator)) (* 2 A)))
                        (l/point-at line (/ (- (* -1 B) (Math/sqrt indicator)) (* 2 A)))))))

;; There is the special case where the line is vertical. This leads to
;; y = q + sqrt(r² - (x-p)²)
(defn intersect-vertical-line
  "Intersect circle and a vertical line."
  [circle line]
  (let [xp2 (* (- (:x (:p1 line)) (:x (:p circle))) (- (:x (:p1 line)) (:x (:p circle))))
        sqr (Math/sqrt (- (* (:r circle) (:r circle)) xp2))]
    (distinct
      (list
        (p/point (:x (:p1 line)) (+ (:y (:p circle)) sqr))
        (p/point (:x (:p1 line)) (- (:y (:p circle)) sqr))))))

(defn intersect-line
  "Intersect circle and line."
  [circle line]
  (if (l/vertical? line)
    (intersect-vertical-line circle line)
    (intersect-non-vertical-line circle line)))

(defn intersect-line-segment
  "Intersect circle and line interpreted as a segment."
  [circle line]
  (filter #(l/point-on-segment? line %) (intersect-line circle line)))

(defn outer-box
  "Get the rectangle surrounding the circle."
  [circle]
  (r/rectangle
    (p/point (- (:x (:p circle)) (:r circle)) (- (:y (:p circle)) (:r circle)))
    (p/point (+ (:x (:p circle)) (:r circle)) (+ (:y (:p circle)) (:r circle)))))

(defn point-in?
  [circle point]
  (< (p/distance (:p circle) point) (:r circle)))
