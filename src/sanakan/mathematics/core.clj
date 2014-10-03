(ns sanakan.mathematics.core
  (:require [sanakan.mathematics.geometry.geometry :as geometry]
            [sanakan.mathematics.voronoi :as voronoi]
            [quil.core :as processing])
  (:gen-class))

(def sites (atom (voronoi/voronoi (list
                   (struct-map geometry/point2 :x 200 :y 300)
                   (struct-map geometry/point2 :x 400 :y 350)
                   (struct-map geometry/point2 :x 500 :y 400)))))
(def sweepline (atom (struct-map geometry/line :a 0 :b 280)))

(defn draw-site
  [site]
  (processing/stroke-float 255 0 0)
  (processing/fill-float 255 0 0)
  (processing/line 0 0 (:x site) (:y site))
  (processing/rect (:x site) (:y site) 2 2))

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
  (dorun (draw-beachline))
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
(defn mouse-released [evt]
  (let [mx (.getX evt)
        my (.getY evt)]
    (reset! sites (voronoi/voronoi (cons (struct-map geometry/point2 :x mx :y my) @sites)))))

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
                  :mouse-moved mouse-moved
                  :mouse-dragged mouse-dragged
                  :mouse-pressed mouse-pressed
                  :key-pressed key-pressed
                  :mouse-released mouse-released))

