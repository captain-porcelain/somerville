(ns somerville.maps.gaia.gui
  (:require [somerville.geometry.line :as line]
            [somerville.geometry.commons :as gcommons]
    [somerville.geometry.polygon :as polygon]
            [somerville.geometry.point :as point]
            [somerville.maps.gaia.core :as gaia]
            [quil.core :as quil])
  (:gen-class))

(def width 600)
(def height 600)
(def color {:r 128 :g 40 :b 20})

;(def world (gaia/triangles (gaia/cube 200)))
(def world (gaia/triangles (gaia/subdivide (gaia/cube 200))))
;(def world (gaia/triangles (gaia/icosahedron 400)))
;(def world (gaia/triangles (gaia/subdivide (gaia/icosahedron 400))))
;(def world (gaia/triangles (gaia/subdivide (gaia/subdivide (gaia/icosahedron 400)))))
;(def world (gaia/random-area-lines 300 16))

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

(defn draw-line
  "Draws one polygon representing an area of the world."
  [l]
  (quil/fill-float 40 40 40 128)
  (quil/stroke-float (:r color) (:g color) (:b color))
  (quil/begin-shape :lines)
  (quil/vertex (:x (:p1 l)) (:y (:p1 l)) (:z (:p1 l)))
  (quil/vertex (:x (:p2 l)) (:y (:p2 l)) (:z (:p2 l)))
  (quil/end-shape))

(defn draw-triangle
  "Draws one polygon representing an area of the world."
  [t]
  (let [ps (polygon/to-points t)]
    (quil/fill-float 40 40 40 128)
    (quil/stroke-float (:r color) (:g color) (:b color))
    (quil/begin-shape :triangles)
    (quil/vertex (:x (nth ps 0)) (:y (nth ps 0)) (:z (nth ps 0)))
    (quil/vertex (:x (nth ps 1)) (:y (nth ps 1)) (:z (nth ps 1)))
    (quil/vertex (:x (nth ps 2)) (:y (nth ps 2)) (:z (nth ps 2)))
    (quil/end-shape)))

(defn draw
  "This function is called by processing repeatedly."
  []
  (quil/background-float 0)
  (quil/with-translation [(/ 800 2) (/ 800 2)]
    (quil/with-rotation [(get-mouse-angle-y 800) 1 0 0]
      (quil/with-rotation [(get-mouse-angle-x 800) 0 1 0]
        (quil/with-rotation [(/ 0 tau) 0 0 1]
          (dorun
            (for [l world]
              (draw-triangle l)
              )))))))

(defn show []
  (quil/sketch
    :title "voronoi"
    :setup setup
    :draw draw
    :size [width height]
    :renderer :p3d
    :mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released))

