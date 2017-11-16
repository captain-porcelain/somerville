(ns somerville.geometry.circle
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.line :as l]
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
(defn intersect-line
  "Intersect circle and line."
  [c l]
  (let [sl (l/slope-intercept l)
        A (+ (* (:a sl) (:a sl)) 1)
        B (* 2 (- (* (:a sl) (:b sl)) (* (:a sl) (:y (:p c))) (:x (:p c))))
        C (+ (- (* (:y (:p c)) (:y (:p c))) (* (:r c) (:r c)) (* (:x (:p c))) (:x (:p c)) (* 2 (:b sl) (:y (:p c)))) (* (:b sl) (:b sl)))
        indicator (- (* B B) (* 4 A C))]
    (cond
      (< indicator 0) (list)
      (= indicator 0) (list (l/point-at l (/ (* -1 B) (* 2 A))))
      (> indicator 0) (list
                        (l/point-at l (/ (+ (* -1 B) (Math/sqrt indicator)) (* 2 A)))
                        (l/point-at l (/ (- (* -1 B) (Math/sqrt indicator)) (* 2 A)))))))

(defn intersect-line-segment
  "Intersect circle and line interpreted as a segment."
  [c l]
  (filter #(l/point-on-segment? l %) (intersect-line c l)))
