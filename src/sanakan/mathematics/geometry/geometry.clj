(ns sanakan.mathematics.geometry.geometry)

;; This file contains functions for handling geometric data.

(defstruct parabola :a :b :c)
(defstruct point2 :x :y)
(defstruct line :a :b)

(defn solve-line-at
  "A line is given by y = a*x + b. This function solves this for a given x."
  [line x]
  (+ (* (:a line) x) (:b line)))

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
        directrix-y (solve-line-at directrix x)
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
  "Plainly subtract one quadratic function from another"
  [parabola1 parabola2]
  (let [a (- (:a parabola1) (:a parabola2))
        b (- (:b parabola1) (:b parabola2))
        c (- (:c parabola1) (:c parabola2))]
    (struct-map parabola :a a :b b :c c)))

(defn solve-parabola-at
  "Solve the quadratic function representing a parabola for a given x."
  [parabola x]
  (+ (* x x (:a parabola)) (* x (:b parabola)) (:c parabola)))

(defn find-zero-of-parabola
  "Find the points where a parabolas value is 0."
  [p]
  (let [dif (struct-map parabola :a 1 :b (/ (:b p) (:a p)) :c (/ (:c p) (:a p)))
        dis (discriminate dif)
        firstpart (* -0.5 (:b dif))]
    (if (> 0 dis)
      ;; negative discriminant means no intersections.
      (list)
      (if (= 0 dis)
        ;; discriminant of 0 means only one intersection.
        (list firstpart)
        (let [x1 (- firstpart (Math/sqrt dis))
              x2 (+ firstpart (Math/sqrt dis))]
          (if (< x1 x2)
            ;; otherwise we have to intersections which we sort by x for convenience.
            (list x1 x2)
            (list x2 x1)))))))

(defn find-zero-of-line
  "Find the point where a line is 0."
  [line]
  (if (= 0 (:a line)) nil (/ (* -1 (:b line)) (:a line))))

(defn intersect-two-parabolas
  "Find the points where two parabolas intersect if such points exist."
  [parabola1 parabola2]
  (let [sub (subtract parabola1 parabola2)
        xs (if (not (= (:a sub) 0))
             (find-zero-of-parabola sub)
             (let [line-zero (find-zero-of-line (struct-map line :a (:b sub) :b (:c sub)))]
               (if (nil? line-zero) (list) (list line-zero))))]
    (map #(struct-map point2 :x % :y (solve-parabola-at parabola1 %)) xs)))

(defn smaller-parabola
  "Given two parabolas return the one that has the smaller value at x"
  [p1 p2 x]
  (if (< (solve-parabola-at p1 x) (solve-parabola-at p2 x)) p1 p2))

(defn beachline-part
  [p1 p2 is]
  (if (= 0 (count is))
    (list (smaller-parabola p1 p2 0))
    (if (= 1 (count is))
      (list
        (smaller-parabola p1 p2 (- (:x (first is)) 1))
        (smaller-parabola p1 p2 (+ (:x (first is)) 1)))
      (list
        (smaller-parabola p1 p2 (- (:x (first is)) 1))
        (smaller-parabola p1 p2 (/ (+ (:x (first is)) (:x (second is))) 2))
        (smaller-parabola p1 p2 (+ (:x (second is)) 1))))))

(defn append-beachline-parts
  [old-parts new-parts]
  (if (= (last old-parts) (first new-parts))
    (concat old-parts (rest new-parts))
    (concat old-parts new-parts)))

(defn beachline
  "Find the points where a list of parabolas intersect. We expect the list to be sorted
  by increasing x of the focal point of the parabolas."
  [parabolas]
  (loop [cur (first parabolas)
         rst (rest parabolas)
         intersections (list)
         ps (list)]
    (if (= 0 (count rst))
      [intersections ps]
      (let [frst (first rst)
            is (intersect-two-parabolas cur frst)]
        (recur frst (rest rst) (concat intersections is) (append-beachline-parts ps (beachline-part cur frst is)))))))

(defn get-parabola-from-beachline
  [intersections parabolas x]
  (let [bigger (count (filter #(> (:x %) x) intersections))
        n (- (count intersections) bigger)]
    (nth parabolas n)))

(defn solve-beachline-at-old
  [intersections parabolas x]
  (solve-parabola-at (get-parabola-from-beachline intersections parabolas x) x))

(defn solve-beachline-at
  [sites sweepline x]
  (first (sort (map #(solve-parabola-at % x) (map #(parabola-from-focuspoint-and-directrix % sweepline) sites)))))
