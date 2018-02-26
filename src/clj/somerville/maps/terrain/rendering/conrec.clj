;; See http://paulbourke.net/papers/conrec/
(ns somerville.maps.terrain.rendering.conrec
  (:require
    [somerville.maps.grid :as grid]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.commons :as c]))

(defrecord TriangleHeights [p0 p1 p2 p3 p4]
  c/Printable
  (c/out [this i] (str (c/indent i) "Triangulation of points\n"
                       (c/out p1 (inc i)) "\n"
                       (c/out p2 (inc i)) "\n"
                       (c/out p3 (inc i)) "\n"
                       (c/out p4 (inc i)) "\nwith center\n"
                       (c/out p0 (inc i)) "\n"))
  (c/out [this] (c/out this 0)))

(defn triangle-heights
  "For a point the the 3 neighboring points with increasing x and y and create 4 triangles.
  The center point receives the average height value."
  [g x y]
  (let [z1 (grid/get-from g x y)
        z2 (grid/get-from g (inc x) y)
        z3 (grid/get-from g (inc x) (inc y))
        z4 (grid/get-from g x (inc y))
        z0 (/ (+ z1 z2 z3 z4) 4)
        p0 (p/point (+ x 0.5) (+ y 0.5) z0)
        p1 (p/point x y z1)
        p2 (p/point (inc x) y z2)
        p3 (p/point (inc x) (inc y) z3)
        p4 (p/point x (inc y) z4)]
    (TriangleHeights. p0 p1 p2 p3 p4)))

(defn triangulation
  "Triangulate grid by creating 4 triangles for each point and its three neighbors."
  [g]
  (let [size (dec (:width g))]
    (for [y (range size)
          x (range size)]
      (triangle-heights g x y))))

(defn relative-heights
  "Convert TriangleHeights to be relative to given height."
  [triangle-heights height]
  (let [hp (p/point 0 0 height)]
    (map #(TriangleHeights.
            (p/subtract (:p0 %) hp)
            (p/subtract (:p1 %) hp)
            (p/subtract (:p2 %) hp)
            (p/subtract (:p3 %) hp)
            (p/subtract (:p4 %) hp))
         triangle-heights)))

(defn h-case
  [p]
  (cond
    (> (:z p) 0) :above
    (< (:z p) 0) :below
    :else        :on))

(defn triangle-case-indices
  "Create list of case indices for each point of triangle heights."
  [triangle]
  [(h-case (:p0 triangle))
   (h-case (:p1 triangle))
   (h-case (:p2 triangle))
   (h-case (:p3 triangle))
   (h-case (:p4 triangle))])


(defn triangle-case-old
  "Collapse case indices for one triangle in the triangle heights for better comparability in case statement."
  [case-indices]
  (case case-indices
    [0 0 0] 0
    [0 0 1] 0
    [0 0 2] 8
    [0 1 0] 0
    [0 1 1] 2
    [0 1 2] 5
    [0 2 0] 7
    [0 2 1] 6
    [0 2 2] 9

    [1 0 0] 0
    [1 0 1] 3
    [1 0 2] 4
    [1 1 0] 1
    [1 1 1] 3
    [1 1 2] 1
    [1 2 0] 4
    [1 2 1] 3
    [1 2 2] 0

    [2 0 0] 9
    [2 0 1] 6
    [2 0 2] 7
    [2 1 0] 5
    [2 1 1] 2
    [2 1 2] 0
    [2 2 0] 8
    [2 2 1] 0
    [2 2 2] 0))

(defn triangle-case
  "Collapse case indices for one triangle in the triangle heights for better comparability in case statement."
  [case-indices]
  (case case-indices
    [:below :below :below] :no-line
    [:below :below :on]    :no-line
    [:below :below :above] :l2->l3
    [:below :on    :below] :no-line
    [:below :on    :on]    :p2->p3
    [:below :on    :above] :p2->l3
    [:below :above :below] :l1->l2
    [:below :above :on]    :p3->l1
    [:below :above :above] :l3->l1

    [:on    :below :below] :no-line
    [:on    :below :on]    :p3->p1
    [:on    :below :above] :p1->l2
    [:on    :on    :below] :p1->p2
    [:on    :on    :on]    :p3->p1
    [:on    :on    :above] :p1->p2
    [:on    :above :below] :p1->l2
    [:on    :above :on]    :p3->p1
    [:on    :above :above] :no-line

    [:above :below :below] :l3->l1
    [:above :below :on]    :p3->l1
    [:above :below :above] :l1->l2
    [:above :on    :below] :p2->l3
    [:above :on    :on]    :p2->p3
    [:above :on    :above] :no-line
    [:above :above :below] :l2->l3
    [:above :above :on]    :no-line
    [:above :above :above] :no-line))

(defn triangle-corner-cases
  "Create cases for corners of each triangle in the TriangleHeights."
  [triangle-heights]
  (let [is (triangle-case-indices triangle-heights)
        tc1 [(nth is 1) (nth is 0) (nth is 2)]
        tc2 [(nth is 2) (nth is 0) (nth is 3)]
        tc3 [(nth is 3) (nth is 0) (nth is 4)]
        tc4 [(nth is 4) (nth is 0) (nth is 1)]]
    [tc1 tc2 tc3 tc4]))

(defn triangle-cases
  "Create case for each triangle in the triangle heights."
  [triangle-heights]
  (map triangle-case (triangle-corner-cases triangle-heights)))

(defn x-sect
  ; (h[p2] * xh[p1] - h[p1] * xh[p2]) / (h[p2] - h[p1])
  [p1 p2]
  (/
   (- (* (:z p2) (:x p1)) (* (:z p1) (:x p2)))
   (- (:z p2) (:z p1))))

(defn y-sect
  ; (h[p2] * yh[p1] - h[p1] * yh[p2]) / (h[p2] - h[p1]);
  [p1 p2]
  (/
   (- (* (:z p2) (:y p1)) (* (:z p1) (:y p2)))
   (- (:z p2) (:z p1))))

(defn intersect
  [p1 p2]
  (p/point (x-sect p1 p2) (y-sect p1 p2)))

(defn case-line
  "Create lines for one triangle in a TriangleHeights by case."
  [p1 p2 p3 c]
  (case c
    :p1->p2 (l/line p1 p2)
    :p2->p3 (l/line p2 p3)
    :p3->p1 (l/line p3 p1)
    :p1->l2 (l/line p1 (intersect p2 p3))
    :p2->l3 (l/line p2 (intersect p3 p1))
    :p3->l1 (l/line p3 (intersect p1 p2))
    :l1->l2 (l/line (intersect p1 p2) (intersect p2 p3))
    :l2->l3 (l/line (intersect p2 p3) (intersect p3 p1))
    :l3->l1 (l/line (intersect p3 p1) (intersect p1 p2))
    nil))

(defn triangle-lines
  "Get lines for each triangle inside a TriangleHeights."
  [t]
  (let [cases (triangle-cases t)]
    [(case-line (:p1 t) (:p0 t) (:p2 t) (nth cases 0))
     (case-line (:p2 t) (:p0 t) (:p3 t) (nth cases 1))
     (case-line (:p3 t) (:p0 t) (:p4 t) (nth cases 2))
     (case-line (:p4 t) (:p0 t) (:p1 t) (nth cases 3))]))

(defn height-lines
  "Get lines for one height value from triangulation."
  [triangles height]
  (filter #(not (nil? %))
    (loop [sets (map #(triangle-lines %) (relative-heights triangles height))
           lines (list)]
      (if (= 0 (count sets))
        lines
        (recur (rest sets) (conj lines (nth (first sets) 0) (nth (first sets) 1) (nth (first sets) 2) (nth (first sets) 3)))))))

(defn heights
  [g]
  (let [size (:width g)]
    (for [y (range size)
          x (range size)]
      (grid/get-from g x y))))

(defn line-heights
  [g min-distance max-lines]
  (let [hs (heights g)
        min-value (apply min hs)
        max-value (apply max hs)
        distance (- max-value min-value)
        tmp (dorun (println (str "Distance: " distance)))
        tmp (dorun (println (str "Max Lines: " max-lines)))
        step (int (/ distance max-lines))
        step (if (< step min-distance) min-distance step)
        tmp (dorun (println (str "Step: " step)))]
    (for [i (range max-lines)]
      (+ min-value (* (inc i) step)))))

(defn contour
  "Get contours of heights in given steps apart."
  [g steps]
  (let [triangles (triangulation g)]
    (map #(hash-map :height % :lines (height-lines triangles %)) (line-heights g 1 steps))))

