(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.geometry.line :as line]
            [sanakan.mathematics.geometry.point :as p]
            [sanakan.mathematics.geometry.voronoi :as voronoi]
            [quil.core :as quil])
  (:gen-class))

(def points (atom (list)))
(def sites (atom (voronoi/voronoi @points 0 0 800 800)))

(defn draw-intersection
  [i]
  (quil/stroke-float 0 0 255)
  (quil/fill-float 0 0 255)
  (quil/rect (:x (:intersection i)) (:y (:intersection i)) 4 4))

(defn draw-bisector
  [bisector]
  (let [y0 (line/solve-line-at (:line bisector) 0)
        y1 (line/solve-line-at (:line bisector) 800)]
    (quil/stroke-float 0 255 0)
    (quil/fill-float 0 255 0)
    (quil/line 0 y0 800 y1)))

(defn draw-site
  [site]
  (quil/stroke-float 255 0 0)
  (quil/fill-float 255 0 0)
  (quil/rect (:x (:point site)) (:y (:point site)) 4 4)
  ;(dorun
  ;  (for [b (:bisectors site)]
  ;    (draw-bisector b)))
  (dorun
      (for [i (:intersections site)]
        (draw-intersection i))))

(defn draw-cell
  [cell]
  (dorun
      (for [l cell]
        (let []
          (quil/stroke-float 255 255 0)
          (quil/fill-float 255 255 0)
          (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l)))
          ))))

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
    (for [site (:cells @sites)]
      (draw-cell site))))

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 10))

(defn mouse-moved [evt])
(defn mouse-dragged [evt])
(defn mouse-pressed [])
(defn mouse-released []
  (let [mx (quil/mouse-x)
        my (quil/mouse-y)]
    (reset! points (cons (p/point mx my) @points))
    (reset! sites (voronoi/voronoi @points 0 0 800 800))))

(defn key-pressed [evt]
  "Trigger actions on key presses."
  (let [k (.getKeyChar evt)
        code (.getKeyCode evt)]
    (dorun (println (str "pressed code " code ", key " k)))
    ;(if (= code 521) ; +
    ;  )
    ;(if (= code 45) ; -
    ;  )
      ))

(defn -main [& args]
  (quil/sketch
    :title "voronoi"
    :setup setup
    :draw draw
    :size [800 800]
    ;:mouse-moved mouse-moved
    ;:mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    ;:key-pressed key-pressed
    ))

