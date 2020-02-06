;; Implementation of the Diamond Square Algorithm for realistic terrain generation.
;; See https://en.wikipedia.org/wiki/Diamond-square_algorithm
;; For comparisons with other approaches see http://blog.habrador.com/2013/02/how-to-generate-random-terrain.html
(ns somerville.maps.terrain.loki
  (:require
    [taoensso.timbre :as log]
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

(defn average
  "Calculate average value for series of coords."
  [g coords]
  (let [fields (map :z
                    (filter #(not (nil? (:z %)))
                            (map #(grid/get-from g (nth % 0) (nth % 1))
                                 coords)))]
    (/ (reduce + fields) (count fields))))

(defn allowed-range-1
  [avg w roughness restrictions]
  (let [min-val (- avg (* w roughness))
        tmp (log/info (str "min-val: " min-val))
        max-val (+ avg (* w roughness))
        tmp (log/info (str "max-val: " max-val))
        restricted-min (get restrictions :min min-val)
        tmp (log/info (str "restricted-min: " restricted-min))
        restricted-max (get restrictions :max max-val)
        tmp (log/info (str "restricted-max: " restricted-max))
        save-min-1 (if (< min-val restricted-min) restricted-min min-val)
        save-min (if (> save-min-1 restricted-max) restricted-min save-min-1)
        tmp (log/info (str "save-min: " save-min))
        save-max-1 (if (> max-val restricted-max) restricted-max max-val)
        save-max (if (< save-max-1 restricted-min) restricted-max save-max-1)
        tmp (log/info (str "save-max: " save-max))
        ]
    [save-min save-max]))

(defn allowed-range
  [avg w roughness restrictions]
  (let [min-val (- avg (* w roughness))
        ;tmp (log/info (str "min-val: " min-val))
        max-val (+ avg (* w roughness))
        ;tmp (log/info (str "max-val: " max-val))
        restricted-min (get restrictions :min min-val)
        ;tmp (log/info (str "restricted-min: " restricted-min))
        restricted-max (get restrictions :max max-val)
        ;tmp (log/info (str "restricted-max: " restricted-max))
        ]
    (cond
      (> min-val restricted-max) [restricted-min restricted-max]
      (< max-val restricted-min) [restricted-min restricted-max]
      (and (> restricted-max max-val) (< min-val restricted-min)) [restricted-min max-val]
      (and (> max-val restricted-max) (< restricted-min min-val)) [min-val restricted-max]
      (and (> max-val restricted-max) (< min-val restricted-min)) [restricted-min restricted-max]
      :else [min-val max-val])))

(defn random-value
  [avg w roughness restrictions]
  (let [[save-min save-max] (allowed-range avg w roughness restrictions)
        dist (- save-max save-min)]
    (+ save-min (rand-int dist))))

(defn new-value
  [g x y w coords config]
  (random-value (average g coords) w (:roughness config) (:restrictions (grid/get-from g x y))))

(defn set-initial-values
  "Set initial height values on corners."
  [g config]
  (let [m (dec (:size config))]
    (set-z g 0 0 (random-value (rand-int (:max config)) (:size config) (:roughness config) (:restrictions (grid/get-from g 0 0))))
    (set-z g 0 m (random-value (rand-int (:max config)) (:size config) (:roughness config) (:restrictions (grid/get-from g 0 m))))
    (set-z g m 0 (random-value (rand-int (:max config)) (:size config) (:roughness config) (:restrictions (grid/get-from g m 0))))
    (set-z g m m (random-value (rand-int (:max config)) (:size config) (:roughness config) (:restrictions (grid/get-from g m m))))
    g))

(defn diamond-step
  "Handle setting on field as part of a diamond step."
  [g [x y w] config]
  ;(log/info (str "Diamond " w " - " x ", " y)))
  (set-z g x y (new-value g x y w (square-coords x y w) config)))

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
  ;(log/info (str "Square " x ", " y)))
  (set-z g x y (new-value g x y w (diamond-coords x y w) config)))

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
  ([config]
    (let [g (grid/grid (:size config) (:size config))
          tmp (set-initial-values g config)
          tmp (dorun (process g config))]
      (assoc g :config config)))
  ([config mask]
    (let [g (grid/grid (:size config) (:size config))
          tmp (set-initial-values g config)
          tmp (dorun (for [[x y restrictions] mask] (grid/update-cell g x y #(assoc % :restrictions restrictions))))
          tmp (dorun (process g config))]
      (assoc g :config config))))

