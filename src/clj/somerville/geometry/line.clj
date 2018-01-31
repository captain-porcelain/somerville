(ns somerville.geometry.line
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.triangle :as t]
    [taoensso.timbre :as log]))


(defrecord Line2 [p1 p2]
  c/Printable
  (c/out [this i] (str (c/indent i) "Line from " (if (nil? p1) "NIL" (c/out p1)) " to " (if (nil? p2) "NIL" (c/out p2))))
  (c/out [this] (c/out this 0)))

(defn line
  "Get a line from two points."
  [p1 p2]
  ;(when (or (nil? p1) (nil? p2)) (throw (Exception. "Can't create line with null as point.")))
  (Line2. p1 p2))

(defn line-from-slope
  "Get a line from the slope intercept form a * x + b."
  [a b]
  (line (p/point 0 b) (p/point 1 (+ a b))))

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
  (+ (:x (:p1 line)) (* t (- (:x (:p2 line)) (:x (:p1 line))))))

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

(defn point-at
  "For a line given by two points this function returns the point at x."
  [line x]
  (p/point x (solve-line-at line x)))

(defn point-on-segment?
  "Check if given point is on the segment of line given by the lines defining points."
  [line point]
  (if (c/close-to (:x (:p1 line)) (:x (:p2 line)))
    (and
      (c/close-to (:x (:p1 line)) (:x point))
      (<= (- (min (:y (:p1 line)) (:y (:p2 line))) 0) (:y point))
      (>= (+ (max (:y (:p1 line)) (:y (:p2 line))) 0) (:y point)))
    (and
      (<= (- (min (:x (:p1 line)) (:x (:p2 line))) 0) (:x point))
      (>= (+ (max (:x (:p1 line)) (:x (:p2 line))) 0) (:x point))
      (<= (- (min (:y (:p1 line)) (:y (:p2 line))) 0) (:y point))
      (>= (+ (max (:y (:p1 line)) (:y (:p2 line))) 0) (:y point))
      (c/close-to (:y point) (solve-line-at line (:x point))))))
  ;(let [xs (sort (list (:x (:p1 l)) (:x (:p2 l))))
        ;ys (sort (list (:y (:p1 l)) (:y (:p2 l))))]
    ;(and
      ;(<= (- (first xs) c/epsilon) (:x p)) (>= (+ (last xs) c/epsilon) (:x p))
      ;(<= (- (first ys) c/epsilon) (:y p)) (>= (+ (last ys) c/epsilon) (:y p)))))

(defn vertical?
  "Check if a line is vertical."
  [l]
  (= (:x (:p1 l)) (:x (:p2 l))))

(defn parallel?
  "Check if two lines are parallel."
  [l1 l2]
  (try
    (if (or (vertical? l1) (vertical? l2))
      (and (vertical? l1) (vertical? l2))
      (let [s1 (p/slope (:p1 l1) (:p2 l1))
            s1 (if (nil? s1) s1 (+ 0.0 s1))
            s2 (p/slope (:p1 l2) (:p2 l2))
            s2 (if (nil? s2) s2 (+ 0.0 s2))]
        (if
          (or (nil? s1) (nil? s2))
          (= s1 s2)
          (c/close-to s1 s2))))
    (catch Exception e
      (dorun (println "Exception while checking if lines are parallel:\n\t" l1 "\n\t" l2)))))
      ;(log/info (str "Exception while checking if lines are parallel:\n\t" l1 "\n\t" l2)))))

(defn normal
  "Create a line for the normal of a line on the first point of the line."
  [line]
  (let [angle (p/angle (:p1 line) (p/point (+ 1 (:x (:p1 line))) (:y (:p1 line))) (:p2 line))]
    (Line2. (:p1 line) (p/point-at (:p1 line) (+ angle (/ java.lang.Math/PI 2)) 1.0))))

(defn normal2
  "Create a line for the normal of a line on the second point of the line."
  [line]
  (let [angle (p/angle (:p1 line) (p/point (+ 1 (:x (:p1 line))) (:y (:p1 line))) (:p2 line))]
    (Line2. (:p2 line) (p/point-at (:p2 line) (+ angle (/ java.lang.Math/PI 2)) 1.0))))

(defn parallel
  "Create a line parallel to the given one dist away."
  [line dist]
  (let [n1 (normal  line)
        n2 (normal2 line)
        x1 (x-by-t n1 dist)
        y1 (y-by-t n1 dist)
        x2 (x-by-t n2 dist)
        y2 (y-by-t n2 dist)]
    (Line2. (p/point x1 y1) (p/point x2 y2))))

(defn bisector-internal
  "Get the line that bisects two points."
  [p1 p2]
  (let [s (* -1 (/ 1 (p/slope p1 p2)))
        m (p/midpoint p1 p2)
        t (- (:y m) (* s (:x m)))]
    (Line2. (p/point 0 t) (p/point 1 (+ s t)))))

(defn bisector
  "Get the line that bisects two points."
  [p1 p2]
  (if (= (:x p1) (:x p2))
    (if (= (:y p1) (:y p2))
      nil
      (Line2. (p/point (:x p1) (/ (+ (:y p1) (:y p2)) 2)) (p/point (+ 1 (:x p1)) (/ (+ (:y p1) (:y p2)) 2))))
    (if (= (:y p1) (:y p2))
      (Line2. (p/point (/ (+ (:x p1) (:x p2)) 2) (:y p1)) (p/point (/ (+ (:x p1) (:x p2)) 2) (+ 1 (:y p1))))
      (bisector-internal p1 p2))))

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

(defn intersect-parameterized
  "Get intersection point of two parameterized lines."
  [l1 l2]
  (let [p11x (:x (:p1 l1))
        p11y (:y (:p1 l1))
        p12x (:x (:p2 l1))
        p12y (:y (:p2 l1))
        p21x (:x (:p1 l2))
        p21y (:y (:p1 l2))
        p22x (:x (:p2 l2))
        p22y (:y (:p2 l2))
        d2y (- p22y p21y)
        ;tmp (when (or (= 0.0 d2y) (= 0 d2y)) (dorun (println (str "d2y is 0 intersecting\n " (c/out l1) "\nand\n" (c/out l2) "\n"))))
        d2y (if (or (= 0.0 d2y) (= 0 d2y)) 0.000000001 d2y)
        g (+ (- (/ (* (- p11y p21y) (- p22x p21x)) d2y) p11x) p21x)
        h (/ (- (* (- p12x p11x) (- p22y p21y)) (* (- p12y p11y) (- p22x p21x))) d2y)
        ;tmp (when (or (= 0.0 h) (= 0 h)) (dorun (println (str "h is 0 intersecting\n " (c/out l1) "\nand\n" (c/out l2) "\n"))))
        h (if (or (= 0.0 h) (= 0 h)) 0.00000001 h)
        t (/ g h)]
    (p/point (x-by-t l1 t) (y-by-t l1 t))))

(defn intersect
  "Intersect two lines."
  [l1 l2]
  (if (parallel? l1 l2)
    nil
    (if (parallel? l2 (line (p/point 0 0) (p/point 1 0)))
      (intersect-parameterized l2 l1)
      (intersect-parameterized l1 l2))))

(defn intersect-segments
  "Intersect two line segments."
  [l1 l2]
  (let [i (intersect l1 l2)]
    (if (not (nil? i))
      (when (and (point-on-segment? l1 i) (point-on-segment? l2 i)) i)
      ;nil
      (cond
        (= (:p1 l1) (:p1 l2)) (:p1 l1)
        (= (:p1 l1) (:p2 l2)) (:p1 l1)
        (= (:p2 l1) (:p1 l2)) (:p2 l1)
        (= (:p2 l1) (:p2 l2)) (:p2 l1)
        :else nil)
      )))

(defn cuts
  "Get list of intersections of one line with a list of lines."
  [line lines]
  (filter #(not (nil? %)) (map #(intersect line %) lines)))

(defn cuts-segments
  "Get list of intersections of one line with a list of lines."
  [line lines]
  (filter #(not (nil? %)) (map #(intersect-segments line %) lines)))

(defn distance
  "Get distance of point from line."
  [line point]
  (/ (* 2 (t/area (t/triangle (:p1 line) (:p2 line) point))) (p/distance (:p1 line) (:p2 line))))
