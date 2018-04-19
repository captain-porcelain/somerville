(ns somerville.maps.gaia.gui
  (:require [somerville.color.color :as color]
            [somerville.geometry.polygon :as polygon]
            [somerville.maps.gaia.core :as gaia]
            [quil.core :as quil])
  (:gen-class))

(def width 600)
(def height 600)
(def line-color (color/rgba 128 40 20))
(def fill-color (color/rgba 40 40 40 128))
(def focus-line-color (color/rgba 128 200 20))
(def focus-fill-color (color/rgba 80 80 80 128))
(def index (atom 0))

(def world (atom (gaia/icosahedron 400)))

(def tau (* 2 Math/PI))

(def mouse-position (atom [0 0]))
(def position (atom [0 0]))

(defn mouse-dragged
  "calculate position change and store in atom"
  []
  (let [x (quil/mouse-x)
        y (quil/mouse-y)
        [old-x old-y] @mouse-position
        [p-x p-y] @position]
    (reset! mouse-position [x y])
    (reset! position [(+ p-x (- x old-x)) (+ p-y (- y old-y))])))

(defn get-mouse-angle-x
  "get mouse based angle"
  [width]
  (let [[x y] @position]
    (* tau (/ (- x width) width))))

(defn get-mouse-angle-y
  "get mouse based angle"
  [height]
  (let [[x y] @position]
    (* tau (/ (- height y) height))))

(defn mouse-pressed
  "store mouse position in atom"
  []
  (let [x (quil/mouse-x)
        y (quil/mouse-y)]
    (dorun (println (str "mouse pressed at " x " - " y)))
    (reset! mouse-position [x y])))

(defn mouse-released
  "for now we just print a status message"
  []
  (dorun (println "mouse released")))

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 10))

(defn draw-triangle
  "Draws one polygon representing an area of the world."
  [t fc lc]
  (quil/fill-float (:r fc) (:g fc) (:b fc) (:a fc))
  (quil/stroke-float (:r lc) (:g lc) (:b lc) (:a lc))
  (quil/begin-shape :triangles)
  (quil/vertex (:x (:p1 t)) (:y (:p1 t)) (:z (:p1 t)))
  (quil/vertex (:x (:p2 t)) (:y (:p2 t)) (:z (:p2 t)))
  (quil/vertex (:x (:p3 t)) (:y (:p3 t)) (:z (:p3 t)))
  (quil/end-shape))

(defn draw
  "This function is called by processing repeatedly."
  []
  (quil/background-float 0)
  (quil/with-translation [(/ 800 2) (/ 800 2)]
    (quil/with-rotation [(get-mouse-angle-y 800) 1 0 0]
      (quil/with-rotation [(get-mouse-angle-x 800) 0 1 0]
        (quil/with-rotation [(/ 0 tau) 0 0 1]
          (dorun
            (for [l @world]
              (draw-triangle l fill-color line-color)))
          (draw-triangle (nth @world @index) focus-fill-color focus-line-color))))))

(defn key-pressed []
  "Trigger actions on key presses."
  ;(dorun (println (str "Pressed key with code " (quil/key-code))))
  (case (quil/key-code)
    43 (when (< @index (dec (count @world))) (swap! index inc)) ;; +
    45 (when (> @index 0) (swap! index dec)) ;; -
    67 (do (reset! index 0) (reset! world (gaia/cube 200)));; c
    73 (do (reset! index 0) (reset! world (gaia/icosahedron 400))) ;; i
    83 (do (reset! index 0) (reset! world (gaia/subdivide @world))) ;; s
    nil))

(defn show []
  (quil/sketch
    :title "voronoi"
    :setup setup
    :draw draw
    :size [width height]
    :renderer :p3d
    :mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-pressed key-pressed))

