(ns somerville.visualization.voronoi
  (:require [somerville.geometry.line :as line]
            [somerville.geometry.commons :as c]
            [somerville.geometry.point :as p]
            [somerville.geometry.voronoi :as voronoi]
            ;[somerville.rendering.svg :as svg]
            [quil.core :as quil :include-macros true]))

(def width 600)
(def height 600)
(def p1 (p/point 200 250))
(def p2 (p/point 500 450))
(def p3 (p/point 100 500))
(def p4 (p/point 250 50))
(def points (atom (list p1 p2 p3 p4)))
(def sites (atom (voronoi/voronoi @points 0 0 width height)))

(defn draw-intersection
  [i]
  (quil/stroke 0 0 255)
  (quil/fill 0 0 255)
  (quil/rect (:x (:intersection i)) (:y (:intersection i)) 4 4))

(defn draw-bisector
  [bisector]
  (let [y0 (line/solve-line-at (:line bisector) 0)
        y1 (line/solve-line-at (:line bisector) width)]
    (quil/stroke 0 255 0)
    (quil/fill 0 255 0)
    (quil/line 0 y0 width y1)))

(defn draw-site
  [site]
  (quil/stroke 255 0 0)
  (quil/fill 255 0 0)
  (quil/rect (- (:x (:point site)) 2) (- (:y (:point site)) 2) 4 4)
  ;(dorun
  ;  (for [b (:bisectors site)]
  ;    (draw-bisector b)))
  (dorun
      (for [i (:intersections site)]
        (draw-intersection i))))

(def highlighted (atom nil))

(defn draw-cell
  [cell]
  (dorun
    (when (> 5 (p/distance (:point cell) (p/point (quil/mouse-x) (quil/mouse-y))))
      (reset! highlighted cell)))
  (dorun
    (for [l (:lines cell)]
      (let []
        (quil/stroke 255 255 0)
        (quil/fill 255 255 0)
        (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l))))))
  (dorun
    (when (not (nil? @highlighted))
     (for [l (:lines @highlighted)]
        (let []
          (quil/stroke 255 255 255)
          (quil/fill 255 255 255)
          (quil/rect (- (:x (:point @highlighted)) 2) (- (:y (:point @highlighted)) 2) 4 4)
          (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l))))))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background 0)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  (dorun
    (for [site (:points @sites)]
      (draw-site site)))
  (dorun
    (for [site (:cells @sites)]
      (draw-cell site))))

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
    (reset! sites (voronoi/voronoi @points 0 0 width height))))

(defn key-pressed []
  "Trigger actions on key presses."
    ;(dorun (println (str "pressed code " (quil/key-code))))
    (if (= (quil/key-code) 67) ; c
      (let []
		(reset! points (list))
		(reset! sites (voronoi/voronoi @points 0 0 width height))))
    (if (= (quil/key-code) 68) ; d
      (dorun (println (c/out @sites))))
    ;(if (= (quil/key-code) 80) ; p
      ;(svg/voronoi @sites "/tmp/voronoi.svg"))
    )

(defn ^:export show []
  (quil/defsketch voronoi
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    ;:mouse-moved mouse-moved
    ;:mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-pressed key-pressed))

