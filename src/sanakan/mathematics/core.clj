(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.geometry :as geometry]
        [rosado.processing :as processing]
        [rosado.processing.applet :as applet])
  (:gen-class))

(defn draw
  "This function is called by processing repeatedly."
  []
  (processing/background-float 0)
  )

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

