(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.geometry :as geometry]
        [rosado.processing :as processing]
        [rosado.processing.applet :as applet])
  (:gen-class))

(def sites (atom (list (struct-map geometry/point2 :x 500 :y 300) (struct-map geometry/point2 :x 400 :y 400))))
(def sweepline (atom (struct-map geometry/line :a 0 :b 280)))

(defn draw-site
   [site]
    (processing/stroke-float 255 0 0)
    (processing/fill-float 255 0 0)
    (processing/line 0 0 (:x site) (:y site))
    (processing/rect (:x site) (:y site) 2 2))

(defn draw-intersections
  [p1 p2]
  (let [is (geometry/intersect p1 p2)]
    (processing/stroke-float 0 255 0)
    (processing/fill-float 0 255 0)
    (dorun
      (for [i is]
        (let []
          (processing/rect (:x i) (:y i) 10 10))))))

(defn draw-parabola
  [parabola]
  (let [xs (range (processing/width))]
    (processing/stroke-float 255 255 0)
    (processing/fill-float 255 255 0)
    (dorun
      (for [x xs]
        (processing/rect x (geometry/solve-parabola-at parabola x) 1 1)))))

(defn draw-sweepline
  []
  (let [y (geometry/solve-line-at @sweepline 0)
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
  (dorun (draw-sweepline))
  (dorun
    (for [site @sites]
        (draw-parabola (geometry/parabola-from-focuspoint-and-directrix site @sweepline))))
  (dorun (draw-intersections
           (geometry/parabola-from-focuspoint-and-directrix (first @sites) @sweepline)
           (geometry/parabola-from-focuspoint-and-directrix (second @sites) @sweepline)))
  (dorun
    (for [site @sites]
        (draw-site site))))

(defn setup
  "This function is called by processing once before drawing"
  []
  (processing/size 800 800 processing/P2D)
  (processing/smooth)
  (processing/fill 226)
  (processing/framerate 10))

(defn mouse-moved [evt])
(defn mouse-dragged [evt])
(defn mouse-pressed [evt])
(defn mouse-released [evt])
(defn key-pressed [evt])

(applet/defapplet voronoi
           :title "voronoi"
           :setup setup
           :draw draw
           :mouse-moved mouse-moved
           :mouse-dragged mouse-dragged
           :mouse-pressed mouse-pressed
           :key-pressed key-pressed
           :mouse-released mouse-released)

(defn -main [& args]
  (applet/run voronoi))

