(ns sanakan.mathematics.visualization.fill
  (:require [sanakan.mathematics.fills.line-fill :as lf]
            [quil.core :as quil])
  (:gen-class))

(def width 600)
(def height 600)

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  )

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 10))

(defn mouse-pressed [])
(defn mouse-released [])

(defn key-pressed [])

(defn show []
  (quil/sketch
    :title "line fill"
    :setup setup
    :draw draw
    :size [width height]
    ;:mouse-moved mouse-moved
    ;:mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-pressed key-pressed))

