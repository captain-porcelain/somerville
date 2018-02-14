(ns somerville.maps.terrain.gaia
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.maps.grid :as grid]
    [clojure.set :as s]))

(defn set-initial-values
  "Set initial height values on corners."
  [g config]
  (grid/update-cell g 0 0 #(assoc % :height (:max config)))
  (grid/update-cell g 0 (:max config) #(assoc % :height (/ (:max config) 2)))
  (grid/update-cell g (:max config) 0 #(assoc % :height (/ (:max config) 2)))
  (grid/update-cell g (:max config) (:max config) #(assoc % :height (:max config)))
  g)

(defn average
  "Calculate average of values."
  [values]
  (/ (reduce + values) (count values)))

;; Define a tile as an rectangle.
(defrecord Tile [x1 y1 x2 y2 w]
  c/Printable
  (c/out [this i] (str (c/indent i) "Terrain Tile describing rectangle (" x1 "," y1 ") to (" x2 "," y2 "). Width is " w))
  (c/out [this] (c/out this 0)))

(defn tile
  "Create a new tile with calculated width."
  [x1 y1 x2 y2]
  (Tile. x1 y1 x2 y2 (- x2 x1)))

(defn tile-points
  "Get points for corners of tile."
  [t]
  (list (p/point (:x1 t) (:y1 t)) (p/point (:x2 t) (:y1 t)) (p/point (:x2 t) (:y2 t)) (p/point (:x1 t) (:y2 t))))

(defn divide-tile
  "Divide a tile into list of 4 sub tiles."
  ([t limit]
   (let [w (int (/ (:w t) 2))
         x12 (+ (:x1 t) w)
         y12 (+ (:y1 t) w)]
     (if (< limit w)
       (list
        (tile (:x1 t) (:y1 t) x12 y12)
          (tile x12 (:y1 t) (:x2 t) y12)
         (tile x12 y12 (:x2 t) (:y2 t))
        (tile (:x1 t) y12 x12 (:y2 t)))
       (list))))
  ([t]
   (divide-tile t 1)))

(defn tile-values
  "Get height values for tile corners."
  [g t]
  (map #(:height (grid/get-from g (:x %) (:y %))) (tile-points t)))

(defn update-new-points
  [g config points w avg]
  (dorun
    (map #(let [r (/ (- (* 2 (rand-int (* 100 (:roughness config)))) 100) 100)
                v (int (+ avg (* r w 2)))
                ;tmp (dorun (println (str "x: " (:x %)  ", y: " (:y %) ", avg: " avg ", w: " w ", r: " r ", v: " v)))
                ]
            (grid/update-cell g (:x %) (:y %) (fn [c] (assoc % :height v))))
         points)))

(defn subtile-points
  "Create set of points that are new in subtiles."
  [t subtiles]
  (s/difference
    (into #{} (reduce concat (map tile-points subtiles)))
    (into #{} (tile-points t))))

(defn loop-over-tiles
  "Loop over all tiles on grid and generate height values."
  [g config]
  (loop [tiles (list (tile 0 0 (:max config) (:max config)))]
    (when-not (= 0 (count tiles))
      (let [t (first tiles)
            ;tmp (dorun (println (c/out t)))
            avg (average (tile-values g t))
            tmp (update-new-points g config (subtile-points t (divide-tile t 0)) (:w t) avg)]
        (recur (concat (rest tiles) (divide-tile t)))))))

;;=======================================================================================================================
;; Interface of the Gaia Generator

(defn default-config
  "Generate default configuration for a world."
  [detail]
  (let [size (inc (int (Math/pow 2 detail)))]
    {:size      size
     :max       (dec size)
     :roughness 1
     :grid-type :rect}))

(defn world
  "Create a landscape based on config."
  [config]
  (let [g (grid/grid (:size config) (:size config))
        tmp (set-initial-values g config)
        tmp (loop-over-tiles g config)]
    g))

