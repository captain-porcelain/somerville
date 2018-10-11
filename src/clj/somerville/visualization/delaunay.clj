(ns somerville.visualization.delaunay
  (:require [somerville.geometry.line :as line]
            [somerville.geometry.commons :as c]
            [somerville.geometry.point :as p]
            [somerville.geometry.delaunay :as delaunay]
            [somerville.geometry.rendering.svg :as svg]
            [quil.core :as quil])
  (:gen-class))

(def width 600)
(def height 600)
(def p1 (p/point 200 250))
(def p2 (p/point 500 450))
(def p3 (p/point 100 500))
(def p4 (p/point 250 50))
(def points (atom (list p1 p2 p3 p4)))
(def delaunay-triangles (atom (delaunay/delaunay @points)))
(def voronoi-lines (atom (delaunay/voronoi @delaunay-triangles)))

(defn draw-point
  [p]
  (quil/stroke-float 0 128 255)
  (quil/fill-float 0 128 255)
  (quil/rect (:x p) (:y p) 4 4))

(defn draw-triangle
  [t]
  (quil/stroke-float 222 128 128)
  (quil/fill-float 222 128 128)
  (quil/rect (:x (:p (:c t))) (:y (:p (:c t))) 4 4)
  (quil/line (:x (:p1 (:t t))) (:y (:p1 (:t t))) (:x (:p2 (:t t))) (:y (:p2 (:t t))))
  (quil/line (:x (:p2 (:t t))) (:y (:p2 (:t t))) (:x (:p3 (:t t))) (:y (:p3 (:t t))))
  (quil/line (:x (:p3 (:t t))) (:y (:p3 (:t t))) (:x (:p1 (:t t))) (:y (:p1 (:t t)))))

(defn draw-line
  [l]
  (quil/stroke-float 22 255 50)
  (quil/fill-float 22 255 50)
  (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (dorun
    (for [p @points]
      (draw-point p)))
  (dorun
    (for [t @delaunay-triangles]
      (draw-triangle t)))
  (dorun
    (for [l @voronoi-lines]
      (draw-line l))))

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 10))

(defn mouse-pressed [])
(defn mouse-released []
  (let [mx (quil/mouse-x)
        my (quil/mouse-y)]
    (reset! points (cons (p/point mx my) @points))
    (reset! delaunay-triangles (delaunay/delaunay @points))
    (reset! voronoi-lines (delaunay/voronoi @delaunay-triangles))))

(defn key-pressed []
  "Trigger actions on key presses."
    ;(dorun (println (str "pressed code " (quil/key-code))))
    (if (= (quil/key-code) 67) ; c
      (let []
        (reset! points (list))
        (reset! delaunay-triangles (list))
        (reset! voronoi-lines (list)))))

(defn show []
  (quil/sketch
    :title "voronoi"
    :setup setup
    :draw draw
    :size [width height]
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-pressed key-pressed))

