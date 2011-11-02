(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.geometry :as geometry]
        [rosado.processing :as processing]
        [rosado.processing.applet :as applet])
  (:gen-class))

(def sites (atom (list (struct-map geometry/point2 :x 400 :y 400))))

(defn draw-site
   [site]
    (processing/stroke-float 255 0 0)
    (processing/fill-float 255 0 0)
    (processing/line 0 0 (:x site) (:y site))
    (processing/rect (:x site) (:y site) 20 20))

(defn draw
  "This function is called by processing repeatedly."
  []
  (processing/background-float 0)
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

