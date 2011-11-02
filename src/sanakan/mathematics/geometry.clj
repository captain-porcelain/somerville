(ns sanakan.mathematics.geometry)

;; This file contains functions for handling geometric data.

(defstruct parabola :a :b :c)
(defstruct point2 :x :y)
(defstruct line :a :b)

(defn parabola-from-factors
  "Create a parabola from the factors of ax² + by + c = 0"
  [a b c]
  (struct-map parabola :a a :b b :c c))

(defn parabola-from-focuspoint-and-directrix
  "Create a parabola such that it defines all points that are
  equidistant from the directrix and the focuspoint. The created
  directrix is open towards the positive y."
  [point directrix]
  (let [x (:x point)
        directrix-y (+ (* (:a directrix) x) (:b directrix))
        distance (- (:y point) directrix-y)
        y (- (:y point) (/ distance 2))
        a (/ 1 (* 2 distance))
        b (/ (* -1 x) distance)
        c (+ y (/ (* x x) (* 2 distance)))]
    (struct-map parabola :a a :b b :c c)))

(defn discriminate
  "The solution for a quadratic formula is the p-q formula: x1,x2 = - p/2 +- sqrt((p/2)² - q).
  The disciminate is the term (p/2)² - q. If a and b of the parabola are 0 the result is undefined so -1 is returned."
  [parabola]
  (let [a (:a parabola)
        p (if (= 0 a) (:b parabola) (/ (:b parabola) a))
        q (if (= 0 a) (:c parabola) (/ (:c parabola) a))
        p-half (/ p 2)
        dis (- (* p-half p-half) q)]
    (if (and (= 0 (:a parabola)) (= 0 (:b parabola))) -1 dis)))

(defn subtract
  ""
  [parabola1 parabola2]
  (let [a (- (:a parabola1) (:a parabola2))
        b (- (:b parabola1) (:b parabola2))
        c (- (:c parabola1) (:c parabola2))]
  (struct-map parabola :a a :b b :c c)))

(defn solve-parabola-at
  [parabola x]
  (+ (* x x (:a parabola)) (* x (:b parabola)) (:c parabola)))

(defn intersect
  ""
  [parabola1 parabola2]
  (let [dif (subtract parabola1 parabola2)
        dis (discriminate dif)
        firstpart (* -0.5 (:b dif))]
    (if (> 0 dis)
      ;; negative discriminant means no intersections.
      (list)
      (if (= 0 dis)
        ;; discriminant of 0 means only one intersection.
        (list (struct-map point2 :x firstpart :y (solve-parabola-at parabola1 firstpart)))
        (let [x1 (- firstpart (Math/sqrt dis))
              y1 (solve-parabola-at parabola1 x1)
              x2 (+ firstpart (Math/sqrt dis))
              y2 (solve-parabola-at parabola1 x2)]
          (if (< x1 x2)
            ;; otherwise we have to intersections which we sort by x for convenience.
            (list (struct-map point2 :x x1 :y y1) (struct-map point2 :x x2 :y y2))
            (list (struct-map point2 :x x2 :y y2) (struct-map point2 :x x1 :y y1))))))))
