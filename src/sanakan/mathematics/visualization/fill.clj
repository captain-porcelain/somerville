(ns sanakan.mathematics.visualization.fill
  (:require [sanakan.mathematics.fills.line-fill :as lf]
            [quil.core :as quil])
  (:gen-class))

(def width 320)
(def height 320)
(def test-image (atom nil))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (quil/image @test-image 0 0))

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 10)
  (reset! test-image (quil/load-image  "/home/sanakan/code/mathematics/resources/test-image.jpg")))

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

