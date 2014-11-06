(ns sanakan.mathematics.geometry.line
  (:require
    [sanakan.mathematics.geometry.commons :as c]
    [sanakan.mathematics.geometry.point :as p]))

;; Define a line by two points
(defrecord Line2 [p1 p2]
  c/Printable
  (c/out [this i] (str (c/indent i) "Line from " (c/out p1) " to " (c/out p2)))
  (c/out [this] (c/out this 0)))

(defn line
  "Get a line from two points."
  [p1 p2]
  (Line2. p1 p2))

(defn slope-intercept
  "Get line in slope intercept form f(x) = a*x + b"
  [line]
  (let [s (p/slope (:p1 line) (:p2 line))]
    {:a s :b (- (:y (:p1 line)) (* s (:x (:p1 line))))}))

(defn solve-line-at-sloped
  "A line is given by y = a*x + b. This function solves this for a given x."
  [line x]
  (let [si (if (nil? (:a line)) (slope-intercept line) line)]
    (+ (* (:a si) x) (:b si))))

(defn parameter-by-x
  "For a line in parameterized form find the parameter value representing x"
  [line x]
  (if (= (:x (:p1 line)) (:x (:p2 line)))
    nil
    (/ (- x (:x (:p1 line))) (- (:x (:p2 line)) (:x (:p1 line))))))

(defn x-by-t
  "For a parameterized line solve it for a given parameter"
  [line t]
  (+ (:y (:p1 line)) (* t (- (:x (:p2 line)) (:x (:p1 line))))))

(defn y-by-t
  "For a parameterized line solve it for a given parameter"
  [line t]
  (+ (:y (:p1 line)) (* t (- (:y (:p2 line)) (:y (:p1 line))))))

(defn solve-line-at
  "For a line given by two points this function solves this for a given x."
  [line x]
  (let [t (parameter-by-x line x)]
    (if (nil? t)
      nil
      (y-by-t line t))))

(defn bisector
  "Get the line that bisects two points."
  [p1 p2]
  (let [s (* -1 (/ 1 (p/slope p1 p2)))
        m (p/midpoint p1 p2)
        t (- (:y m) (* s (:x m)))]
    (Line2. (p/point 0 t) (p/point 1 (+ s t)))))

(defn intersect-sloped
  "Get intersection point of two lines."
  [l1 l2]
  (let [si1 (slope-intercept l1)
        si2 (slope-intercept l2)]
    (if (= (:a si1) (:a si2))
      nil
      (let [x (/ (- (:b si2) (:b si1)) (- (:a si1) (:a si2)))
            y (solve-line-at l1 x)]
        (p/point x y)))))

(defn parallel?
  "Check if two lines are parallel."
  [l1 l2]
  (= (p/slope (:p1 l1) (:p2 l1)) (p/slope (:p1 l2) (:p2 l2))))

(defn intersect
  "Get intersection point of two parameterized lines."
  [l1 l2]
  (if (parallel? l1 l2)
    nil
    (let [p11x (:x (:p1 l1))
          p11y (:y (:p1 l1))
          p12x (:x (:p2 l1))
          p12y (:y (:p2 l1))
          p21x (:x (:p1 l2))
          p21y (:y (:p1 l2))
          p22x (:x (:p2 l2))
          p22y (:y (:p2 l2))
          d2y (- p22y p21y)
          d2y (if (or (= 0.0 d2y) (= 0 d2y)) 0.000000001 d2y)
          g (+ (- (/ (* (- p11y p21y) (- p22x p21x)) d2y) p11x) p21x)
          h (/ (- (* (- p12x p11x) (- p22y p21y)) (* (- p12y p11y) (- p22x p21x))) d2y)
          h (if (or (= 0.0 h) (= 0 h)) 0.00000001 h)
          t (/ g h)]
      (p/point (x-by-t l1 t) (y-by-t l1 t)))))

(defn cuts
  "Get list on intersections of one line with a list of lines."
  [line lines]
  (filter #(not (nil? %)) (map #(intersect line %) lines)))
