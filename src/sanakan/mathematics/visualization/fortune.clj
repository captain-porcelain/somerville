(ns sanakan.mathematics.visualization.fortune
  (:require [sanakan.mathematics.geometry.line :as line]
            [sanakan.mathematics.geometry.commons :as c]
            [sanakan.mathematics.geometry.point :as p]
            [sanakan.mathematics.geometry.parabola :as parabola]
            [sanakan.mathematics.geometry.fortune :as fortune]
            [quil.core :as quil])
  (:gen-class))

(def width 600)
(def height 600)
(def p1 (p/point 200 250))
(def p2 (p/point 500 450))
(def p3 (p/point 100 500))
(def p4 (p/point 250 50))
(def points (atom (list p1 p2 p3 p4)))
(def sites (atom (fortune/voronoi @points)))

(defn draw-intersection
  [i]
  (quil/stroke-float 0 0 255)
  (quil/fill-float 0 0 255)
  (quil/rect (:x (:intersection i)) (:y (:intersection i)) 4 4))

(defn draw-bisector
  [bisector]
  (let [y0 (line/solve-line-at (:line bisector) 0)
        y1 (line/solve-line-at (:line bisector) width)]
    (quil/stroke-float 0 255 0)
    (quil/fill-float 0 255 0)
    (quil/line 0 y0 width y1)))

(defn draw-parabola
  [parabola]
  (let [xs (range (quil/width))]
    (quil/stroke-float 255 255 0)
    (quil/fill-float 255 255 0)
    (dorun
      (for [x xs]
        (quil/rect x (parabola/solve-parabola-at parabola x) 1 1)))))

(defn draw-sweepline
  [y]
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (quil/line 0 y width y))

(defn draw-site
  [site]
  (quil/stroke-float 255 0 0)
  (quil/fill-float 255 0 0)
  (quil/rect (- (:x site) 2) (- (:y site) 2) 4 4))

(defn draw-event
  [site]
  (quil/stroke-float 255 255 0)
  (quil/fill-float 255 255 0)
  (quil/rect (- (:x (:point site)) 2) (- (:y (:point site)) 2) 4 4))

(def highlighted (atom nil))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (dorun
    (for [site (:points @sites)]
      (draw-site site)))
  (dorun
    (for [site (:events @sites)]
      (draw-event site)))
  (let [step (- (:step @sites) 1)
        sweep-y (if (>= step 0)
                  (:y (nth (:points @sites) step))
                  nil)]
    (if (not (nil? sweep-y))
      (do
        (dorun
          (for [site (take (:step @sites) (:points @sites))]
            (draw-parabola (parabola/parabola-from-focuspoint-and-directrix-y site (+ 1 sweep-y)))))
        (draw-sweepline sweep-y)))))

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
    (reset! sites (fortune/voronoi @points))))

(defn key-pressed []
  "Trigger actions on key presses."
  ;(dorun (println (str "pressed code " (quil/key-code))))
  (if (= (quil/key-code) 32) ; space for progressing a step
    (reset! sites (fortune/step @sites)))
  (if (= (quil/key-code) 67) ; c for clearing
    (do
      (reset! points (list))
      (reset! sites (fortune/voronoi @points))))
  (if (= (quil/key-code) 68) ; d for debugging
    (dorun (println (c/out @sites))))
  (if (= (quil/key-code) 82) ; r for resetting to step 0
    (reset! sites (fortune/voronoi @points))))

(defn show []
  (quil/sketch
    :title "voronoi"
    :setup setup
    :draw draw
    :size [width height]
    ;:mouse-moved mouse-moved
    ;:mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-pressed key-pressed))

