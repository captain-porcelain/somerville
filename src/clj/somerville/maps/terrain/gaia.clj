(ns somerville.maps.terrain.gaia
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.maps.grid :as grid]))

(defn set-initial-values
  "Set initial height values on corners."
  [g config]
  (grid/update-cell g 0 0 #(assoc % :height (:max config)))
  (grid/update-cell g 0 (:max config) #(assoc % :height (/ (:max config) 2)))
  (grid/update-cell g (:max config) 0 #(assoc % :height (/ (:max config) 2)))
  (grid/update-cell g (:max config) (:max config) #(assoc % :height (:max config)))
  g)

(defn random-offset
  [scale]
  (- (* (/ (rand-int 100) 100) scale 2) scale))

(defn average
  [values]
  (/ (reduce + values) (count values)))

(defn in-range
  [config [x y]]
  (and (< -1 x (:size config)) (< -1 y (:size config))))

(defn generate-value
  [g config offset coords]
  (let [relevant (filter #(in-range config %) coords)
        heights (map #(get (grid/get-from g %) :height 0) relevant)]
    (+ offset (average heights))))

(defn square-coords
  [x y tile-size]
  [[(- x tile-size) (- y tile-size)]
   [(+ x tile-size) (- y tile-size)]
   [(+ x tile-size) (+ y tile-size)]
   [(- x tile-size) (+ y tile-size)]])

(defn square
  [g config tile-size offset x y]
  (let [coords (square-coords x y tile-size)
        value (generate-value g config offset coords)]
    (grid/update-cell g x y #(assoc % :height value))))

(defn diamond-coords
  [x y tile-size]
  [[x (- y tile-size)]
   [(+ x tile-size) y]
   [x (+ y tile-size)]
   [(- x tile-size) y]])

(defn diamond
  [g config tile-size offset x y]
  (let [coords (diamond-coords x y tile-size)
        value (generate-value g config offset coords)]
    (grid/update-cell g x y #(assoc % :height value))))

(defn divide-squares
  [g config tile-size half scale]
  (loop [y half]
    (when (< y (:size config))
      (loop [x half]
        (when (< x (:size config))
          (square g config half (random-offset scale) x y)
          (recur (+ x tile-size))))
      (recur (+ y tile-size)))))

(defn divide-diamonds
  [g config tile-size half scale]
  (loop [y 0]
    (when (< y (:size config))
      (loop [x (mod (+ y half) tile-size)]
        (when (< x (:size config))
          (diamond g config half (random-offset scale) x y)
          (recur (+ x tile-size))))
      (recur (+ y half)))))

(defn divide-tiles
  [g config tile-size]
  (let [half  (/ tile-size 2)
        scale (* (:roughness config) tile-size)]
    (when-not (< half 1)
      (divide-squares g config tile-size half scale)
      (divide-diamonds g config tile-size half scale)
      (recur g config half))))

;;-----------------------------------------------------------------------------------------------------------------------

(defn divide
  [rect]
  (let [x1 (:x (nth rect 0))
        x2 (:x (nth rect 1))
        y1 (:y (nth rect 1))
        y2 (:y (nth rect 3))
        w (int (/ (- x2 x1) 2))]
    (if (<= 2 w)
      (list
        [(p/point    x1       y1)    (p/point (+ x1 w)    y1)    (p/point (+ x1 w) (+ y1 w)) (p/point    x1    (+ y1 w))]
        [(p/point (+ x1 w)    y1)    (p/point    x2       y1)    (p/point    x2    (+ y1 w)) (p/point (+ x1 w) (+ y1 w))]
        [(p/point (+ x1 w) (+ y1 w)) (p/point    x2    (+ y1 w)) (p/point    x2       y2   ) (p/point (+ x1 w)    y2   )]
        [(p/point    x1    (+ y1 w)) (p/point (+ x1 w) (+ y1 w)) (p/point (+ x1 w)    y2   ) (p/point    x1       y2   )])
      (list))))

(defn initial-rectangle
  [config]
  [(p/point 0 0) (p/point (:max config) 0) (p/point (:max config) (:max config)) (p/point 0 (:max config))])

(defn rect-values
  [g rect]
  (map #(:height (grid/get-from g (:x %) (:y %))) rect))

(defn update-new-points
  [g config points w avg]
  (dorun
    (map #(let [r (/ (- (* 2 (rand-int (* 100 (:roughness config)))) 100) 100)
                v (int (+ avg (* r w 2)))
                ;tmp (dorun (println (str "avg: " avg ", w: " w ", r: " r ", v: " v)))
                ]
            (grid/update-cell g (:x %) (:y %) (fn [c] (assoc % :height v))))
         points)))

(defn new-points
  [w x y]
  (list
    (p/point (+ x w) y)
    (p/point x (+ y w))
    (p/point (+ x w) (+ y w))
    (p/point (+ x w w) (+ y w))
    (p/point (+ x w) (+ y w w))))

(defn loop-over
  [g config]
  (loop [rectangles (list (initial-rectangle config))]
    (when-not (= 0 (count rectangles))
      (let [rect (first rectangles)
            w (int (/ (- (:x (nth rect 1)) (:x (nth rect 0))) 2))
            avg (average (rect-values g rect))
            tmp (update-new-points g config (new-points w (:x (first rect)) (:y (first rect))) w avg)]
        (recur (concat (rest rectangles) (divide rect)))))))

(defn process2
  "Execute landscape generating process on fresh grid."
  [config]
  (let [g (grid/grid (:width config) (:height config))
        tmp (set-initial-values g config)
        tmp (loop-over g config)]
    g))

(defn process
  "Execute landscape generating process on fresh grid."
  [config]
  (let [g (grid/grid (:width config) (:height config))
        tmp (set-initial-values g config)
        tmp (divide-tiles g config (:max config))]
    g))


;;=======================================================================================================================
;; Interface of the Gaia Generator

(defn default-config
  "Generate default configuration for a world."
  [detail]
  (let [size (inc (int (Math/pow 2 detail)))]
    {:size      size
     :max       (dec size)
     :width     size
     :height    size
     :roughness 1
     :grid-type :rect}))

(defn world
  "Create a landscape based on config."
  [config]
  (process config))

