;; Implementation of the Diamond Square Algorithm for realistic terrain generation.
;; See https://en.wikipedia.org/wiki/Diamond-square_algorithm
;; For comparisons with other approaches see http://blog.habrador.com/2013/02/how-to-generate-random-terrain.html
(ns somerville.maps.terrain.loki
  (:require
    [somerville.maps.grid :as grid]
    ))

(defn random-height
  "Create a random height between configured minimum and maximum."
  [config]
  (+ (:min config) (rand-int (- (:max config) (:min config)))))

(defn set-initial-values
  "Set initial height values on corners."
  [g config]
  (let [m (dec (:size config))]
    (grid/set-integer g 0 0 (rand-int (:max config)))
    (grid/set-integer g 0 m (rand-int (:max config)))
    (grid/set-integer g m 0 (rand-int (:max config)))
    (grid/set-integer g m m (rand-int (:max config)))
    g))

(defn default-config
  "Generate default configuration for a world."
  [detail]
  (let [size (inc (int (Math/pow 2 detail)))]
    {:detail    detail
     :size      size
     :min       -10
     :max       25
     :roughness 1}))

(defn diamond-step
  "Handle setting on field as part of a diamond step."
  [g [x y w] config]
  ;(dorun (println (str "Diamond " w " - " x ", " y))))
  (grid/set-integer g x y 1))

(defn diamond-steps
  "Calculate the fields to be set as part of a diamond step."
  [n config]
  (let [steps (int (Math/pow 2 n))
        w (int (/ (dec (:size config)) steps 2))]
      (for [x (range steps)
            y (range steps)]
        [(* (inc (* 2 x)) w) (* (inc (* 2 y)) w) w])))

(defn diamond-average
  [g x y w]
  (let [fields (filter #(not= {} %)
                       (map #(grid/get-from g (nth % 0) (nth % 1))
                            (list [x (- y w)] [(- x w) y] [(+ x w) y] [x (+ y w)])))]
    (/ (reduce + fields) (count fields))))

(defn square-step
  "Handle setting on field as part of a square step."
  [g x y w config]
  ;(dorun (println (str "Square " x ", " y))))
  (grid/set-integer g x y (diamond-average g x y w)))

(defn square-steps
  "Handle the square steps for a given diamond coordinate.
  NOTE: this causes fields to be set multiple times."
  [g [x y w] config]
  (do
    (square-step g    x    (- y w) w config)
    (square-step g (- x w)    y    w config)
    (square-step g (+ x w)    y    w config)
    (square-step g    x    (+ y w) w config)))

(defn process
  "Run the actual diamond-squares algorithm on the given grid."
  [g config]
  (for [n (range (:detail config))]
    (let [diamonds (diamond-steps n config)
          tmp (dorun (map #(diamond-step g % config) diamonds))
          tmp (dorun (map #(square-steps g % config) diamonds))]
      )))

(defn world
  "Create a landscape based on config."
  [config]
  (let [g (grid/integer-grid (:size config) (:size config))
        tmp (set-initial-values g config)
        tmp (dorun (process g config))
        ]
    g))

