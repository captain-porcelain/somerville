(ns somerville.visualization.fortune
  (:require [somerville.geometry.line :as line]
            [somerville.geometry.commons :as c]
            [somerville.geometry.point :as p]
            [somerville.geometry.parabola :as parabola]
            [somerville.geometry.fortune :as fortune]
            [somerville.geometry.voronoi :as voronoi]
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
(def vsites (atom (voronoi/voronoi @points 0 0 width height)))

(defn draw-intersection
  [i]
  (quil/stroke-float 0 0 255)
  (quil/fill-float 0 0 255)
  (quil/rect (p/x (:intersection i)) (p/y (:intersection i)) 4 4))

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
    (quil/stroke-float 237 237 177)
    (quil/fill-float 237 237 177)
    (dorun
      (for [x xs]
        (quil/line x (parabola/solve-parabola-at parabola x) (+ x 1) (parabola/solve-parabola-at parabola (+ x 1)))))))

(defn draw-sweepline
  [y]
  (quil/stroke-float 211 248 226)
  (quil/fill-float 211 248 226)
  (quil/line 0 y width y))

(defn draw-site
  [site]
  (quil/stroke-float 246 148 193)
  (quil/fill-float 246 148 193)
  (quil/rect (- (p/x site) 2) (- (p/y site) 2) 4 4))

(defn draw-event
  [site]
  (quil/stroke-float 211 248 226)
  (quil/fill-float 211 248 226)
  (quil/rect (- (p/x (:point site)) 2) (- (p/y (:point site)) 2) 4 4))

(defn draw-edge
  [edge]
  (quil/stroke-float 169 222 249)
  (quil/fill-float 169 222 249)
  (when (and (not (nil? (:left edge))) (not (nil? (:right edge))))
    (let [m (p/midpoint (:left edge) (:right edge))]
      (quil/line (p/x (:start edge)) (p/y (:start edge)) (p/x m) (p/y m)))))

(defn draw-cell
  [cell]
  (quil/stroke-float 200 200 200)
  (quil/fill-float 200 200 200)
  (dorun (map #(quil/line (p/x (:p1 %)) (p/y (:p1 %)) (p/x (:p2 %)) (p/y (:p2 %))) (:lines cell))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (dorun
	(for [site (:cells @vsites)]
	  (draw-cell site)))
  (dorun
    (for [site (:points @sites)]
      (draw-site site)))
  (dorun
    (for [site (:events @sites)]
      (draw-event site)))
  (dorun
    (for [node (fortune/sequence-from-tree (:tree @sites))]
      (when (not (nil? (:edge node))) (draw-edge (:edge node)))))
  (let [step (- (:step @sites) 1)
        sweep-y (if (and (>= step 0) (< step (count (:points @sites))))
                  (p/y (nth (:points @sites) step))
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
	(reset! vsites (voronoi/voronoi @points 0 0 width height))
    (reset! sites (fortune/voronoi @points))))

(defn key-pressed []
  "Trigger actions on key presses."
  ;(dorun (println (str "pressed code " (quil/key-code))))
  (if (= (quil/key-code) 32) ; space for progressing a step
    (reset! sites (fortune/step @sites)))
  (if (= (quil/key-code) 67) ; c for clearing
    (do
      (reset! points (list))
	  (reset! vsites (voronoi/voronoi @points 0 0 width height))
      (reset! sites (fortune/voronoi @points))))
  (if (= (quil/key-code) 68) ; d for debugging
    (dorun (println (c/out @sites))))
  (if (= (quil/key-code) 82) ; r for resetting to step 0
    (do
	  (reset! vsites (voronoi/voronoi @points 0 0 width height))
      (reset! sites (fortune/voronoi @points)))))

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

