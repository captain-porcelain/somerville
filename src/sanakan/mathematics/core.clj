(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.geometry.geometry :as geometry]
            [sanakan.mathematics.geometry.line :as line]
            [sanakan.mathematics.geometry.point :as p]
            [sanakan.mathematics.geometry.voronoi :as voronoi]
            [quil.core :as processing])
  (:gen-class))

(def points (atom (list)))
(def sites (atom (voronoi/voronoi @points 0 0 800 800)))
(def sweepline (atom (struct-map geometry/line :a 0 :b 280)))

(defn draw-bisector
  [bisector]
  (let [y0 (line/solve-line-at bisector 0)
        y1 (line/solve-line-at bisector 800)]
    (processing/stroke-float 0 255 0)
    (processing/fill-float 0 255 0)
    (processing/line 0 y0 800 y1)))

(defn draw-site
  [site]
  (processing/stroke-float 255 0 0)
  (processing/fill-float 255 0 0)
  ;(processing/line 0 0 (:x site) (:y site))
  (processing/rect (:x (:p site)) (:y (:p site)) 2 2)
  (dorun
    (for [b (:bisectors site)]
      (draw-bisector (:bisector b)))))

(defn draw-beachline
  []
  (let [xs (range (processing/width))]
    (processing/stroke-float 255 255 255)
    (processing/fill-float 255 255 255)
    (dorun
      (for [x xs]
        (processing/rect x (geometry/solve-beachline-at @sites @sweepline x) 1 1)))))

(defn draw-sweepline
  []
  (let [y (line/solve-line-at @sweepline 0)
        ww (processing/width)
        wh (processing/height)]
    (processing/stroke-float 255 0 255)
    (processing/fill-float 255 0 255)
    (processing/line 0 y ww y)))

(defn draw
  "This function is called by processing repeatedly."
  []
  (processing/background-float 0)
  (processing/stroke-float 0 255 0)
  (processing/fill-float 0 255 0)
  ;(dorun (draw-sweepline))
  ;(dorun (draw-beachline))
  (dorun
    (for [site @sites]
      (draw-site site))))

(defn setup
  "This function is called by processing once before drawing"
  []
  (processing/smooth)
  (processing/fill 226)
  (processing/frame-rate 10))

(defn mouse-moved [evt])
(defn mouse-dragged [evt])
(defn mouse-pressed [])
(defn mouse-released []
  (let [mx (processing/mouse-x)
        my (processing/mouse-y)]
    (reset! points (cons (p/point mx my) @points))
    (reset! sites (voronoi/voronoi @points 0 0 800 800))))

(defn key-pressed [evt]
  "Trigger actions on key presses."
  (let [k (.getKeyChar evt)
        code (.getKeyCode evt)]
    (dorun (println (str "pressed code " code ", key " k)))
    (if (= code 521) ; +
      (reset! sweepline (struct-map geometry/line :a 0 :b (+ (:b @sweepline) 1))))
    (if (= code 45) ; -
      (reset! sweepline (struct-map geometry/line :a 0 :b (- (:b @sweepline) 1))))))

(defn -main [& args]
  (processing/sketch
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

