;; See https://www.redblobgames.com/x/1842-delaunay-voronoi-sphere/
(ns somerville.visualization.world
  (:require [somerville.geometry.commons :as commons]
            [somerville.geometry.line :as line]
            [somerville.geometry.point :as point]
            [somerville.geometry.sphere :as sphere]
            [quil.core :as quil])
  (:gen-class))

(def width 600)
(def height 600)
(def points (atom (map #(point/scale % 100) (sphere/fibonacci 100))))

(defn draw-point
  [p]
  (quil/stroke-float 0 128 255)
  (quil/fill-float 0 128 255)
  (quil/with-translation [(:x p) (:y p) (:z p)]
    (quil/sphere 1)))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/camera 300 300 0 0 0 0 0 0 -1)
  (quil/background-float 0)
  (quil/stroke-float 222 0 128)
  (quil/no-fill)
  (quil/rect 0 0 width height)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (dorun
    (for [p @points]
      (draw-point p))))

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 10))

(defn show []
  (quil/sketch
    :title "voronoi"
    :setup setup
    :draw draw
    :size [width height]
    :renderer :p3d
    ;:mouse-pressed mouse-pressed
    ;:mouse-released mouse-released
    ;:key-pressed key-pressed
    ))

