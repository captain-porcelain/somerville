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
(def points (atom (list)))
(def delaunay-triangles (atom (delaunay/delaunay @points (delaunay/bounding-triangle (list (p/point width height))))))
(def voronoi-lines (atom (delaunay/voronoi @delaunay-triangles)))
(def draw-delaunay (atom true))
(def draw-voronoi (atom true))

(defn draw-point
  [p]
  (quil/stroke-float 0 128 255)
  (quil/fill-float 0 128 255)
  (quil/rect (:x p) (:y p) 4 4))

(defn draw-triangle
  [t]
  (quil/stroke-float 222 128 128)
  ;(quil/no-fill)
  ;(quil/ellipse (:x (:p (:c t))) (:y (:p (:c t))) (:r (:c t)) (:r (:c t)))
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
  (when @draw-delaunay
    (dorun
      (for [t @delaunay-triangles]
        (draw-triangle t))))
  (when @draw-voronoi
    (dorun
      (for [l @voronoi-lines]
        (draw-line l)))))

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 10))

(defn mouse-pressed [])
(defn mouse-released []
  (let [mx (quil/mouse-x)
        my (quil/mouse-y)
        pm (p/point mx my)]
    (reset! points (cons pm @points))
    (reset! delaunay-triangles (delaunay/add-point @delaunay-triangles pm))
    (reset! voronoi-lines (delaunay/voronoi @delaunay-triangles))))

(defn key-pressed []
  "Trigger actions on key presses."
    ;(dorun (println (str "pressed code " (quil/key-code))))
    (case (quil/key-as-keyword)
      :c (let []
           (reset! points (list))
           (reset! delaunay-triangles (delaunay/delaunay @points (delaunay/bounding-triangle (list (p/point width height)))))
           (reset! voronoi-lines (list)))
      :d (reset! draw-delaunay (not @draw-delaunay))
      :v (reset! draw-voronoi (not @draw-voronoi))
      (dorun (println (str "pressed key " (quil/key-as-keyword))))))

(defn show []
  (quil/sketch
    :title "voronoi"
    :setup setup
    :draw draw
    :size [width height]
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-pressed key-pressed))

