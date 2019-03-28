;; See https://www.redblobgames.com/x/1842-delaunay-voronoi-sphere/
(ns somerville.visualization.world
  (:require [somerville.geometry.commons :as commons]
            [somerville.geometry.line :as line]
            [somerville.geometry.point :as point]
            [somerville.geometry.sphere :as sphere]
            [somerville.geometry.projection.stereographic :as proj]
            [somerville.geometry.delaunay :as delaunay]
            [quil.core :as quil])
  (:gen-class))

;;============================================================================================================
;; Helpers

(defn line-to-sphere
  "Map the points of a line onto a sphere."
  [l]
  (line/line (proj/to-sphere (:p1 (:line l))) (proj/to-sphere (:p2 (:line l)))))

(defn to-voronoi
  "Create voronoi for points on a sphere."
  [points]
  (map line-to-sphere (delaunay/voronoi (delaunay/delaunay (map proj/to-plane points)))))

;;============================================================================================================
;; Rendering

(def width 600)
(def height 600)
(def points (atom (sphere/fibonacci 100)))
(def lines (atom (to-voronoi @points)))

(defn draw-point
  [p]
  (quil/stroke-float 0 128 255)
  (quil/fill-float 0 128 255)
  (quil/stroke-weight 1)
  (let [sp (point/scale p 100)]
    (quil/with-translation [(:x sp) (:y sp) (:z sp)]
      (quil/sphere 1))))

(defn draw-line
  [l]
  (quil/stroke-float 255 128 0)
  (quil/fill-float 255 128 0)
  (quil/stroke-weight 1)
  (let [p1 (point/scale (:p1 l) 100)
        p2 (point/scale (:p2 l) 100)]
    (quil/line (:x p1) (:y p1) (:z p1) (:x p2) (:y p2) (:z p2))))

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
      (draw-point p)))
  (dorun
    (for [l @lines]
      (draw-line l))))

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

