;; Implementation of the Diamond Square Algorithm for realistic terrain generation.
;; See https://en.wikipedia.org/wiki/Diamond-square_algorithm
;; For comparisons with other approaches see http://blog.habrador.com/2013/02/how-to-generate-random-terrain.html
(ns somerville.maps.terrain.loki
  (:require
    [somerville.maps.grid :as grid]))

;==========================================================================================================================
; Helper Functions

(defn random-height
  "Create a random height between configured minimum and maximum."
  [config]
  (+ (:min config) (rand-int (- (:max config) (:min config)))))

(defn set-z
  "Set z value in grid if not already set."
  [g x y z]
  (let [c (grid/get-from g x y)]
    (when (nil? (:z c))
      (grid/set-in g x y (assoc c :z z)))))

(defn diamond-coords
  "Create diamond coordiantes for center and width."
  [x y w]
  [[   x    (- y w)]
   [(- x w)    y]
   [(+ x w)    y]
   [   x    (+ y w)]])

(defn square-coords
  "Create square coordiantes for center and width."
  [x y w]
  [[(- x w) (- y w)]
   [(+ x w) (- y w)]
   [(- x w) (+ y w)]
   [(+ x w) (+ y w)]])

;==========================================================================================================================
; Actual Algorithm

(defn set-initial-values
  "Set initial height values on corners."
  [g config]
  (let [m (dec (:size config))]
    (set-z g 0 0 (rand-int (:max config)))
    (set-z g 0 m (rand-int (:max config)))
    (set-z g m 0 (rand-int (:max config)))
    (set-z g m m (rand-int (:max config)))
    g))

(defn average
  "Calculate average value for series of coords."
  [g coords]
  (let [fields (map :z
                    (filter #(not (nil? (:z %)))
                            (map #(grid/get-from g (nth % 0) (nth % 1))
                                 coords)))]
    (/ (reduce + fields) (count fields))))

(defn randomize
  [a w config]
  (let [up (= 0 (rand-int 2))
        dist (rand-int
               (*
                w
                (:roughness config)
                ;(if up
                  ;(- (:max config) a)
                  ;(- a (:min config)))
                ))]
    (if up (+ a dist) (- a dist))))

(defn diamond-step
  "Handle setting on field as part of a diamond step."
  [g [x y w] config]
  ;(dorun (println (str "Diamond " w " - " x ", " y))))
  (set-z g x y (randomize (average g (square-coords x y w)) w config)))

(defn diamond-steps
  "Calculate the fields to be set as part of a diamond step."
  [n config]
  (let [steps (int (Math/pow 2 n))
        w (int (/ (dec (:size config)) steps 2))]
    (for [x (range steps)
          y (range steps)]
      [(* (inc (* 2 x)) w) (* (inc (* 2 y)) w) w])))

(defn square-step
  "Handle setting on field as part of a square step."
  [g [x y] w config]
  ;(dorun (println (str "Square " x ", " y))))
  (set-z g x y (randomize (average g (diamond-coords x y w)) w config)))

(defn square-steps
  "Handle the square steps for a given diamond coordinate.
  NOTE: this does not exclude fields that are already set."
  [g [x y w] config]
  (dorun (map #(square-step g % w config) (diamond-coords x y w))))

(defn process
  "Run the actual diamond-squares algorithm on the given grid."
  [g config]
  (for [n (range (:detail config))]
    (let [diamonds (diamond-steps n config)
          tmp (dorun (map #(diamond-step g % config) diamonds))
          tmp (dorun (map #(square-steps g % config) diamonds))]
      )))

;==========================================================================================================================
; Outside Interface

(defn default-config
  "Generate default configuration for a world."
  [detail]
  (let [size (inc (int (Math/pow 2 detail)))]
    {:detail    detail
     :size      size
     :min       -10
     :max       25
     :roughness 0.5}))

(defn world
  "Create a landscape based on config."
  [config]
  (let [g (grid/grid (:size config) (:size config))
        tmp (set-initial-values g config)
        tmp (dorun (process g config))
        ]
    (assoc g :config config)))

