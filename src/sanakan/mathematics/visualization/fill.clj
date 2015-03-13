(ns sanakan.mathematics.visualization.fill
  (:require [sanakan.mathematics.fills.line-fill :as lf]
            [sanakan.mathematics.geometry.point :as p]
            [sanakan.mathematics.image :as i]
            [sanakan.mathematics.color.color :as c]
            [quil.core :as quil])
  (:gen-class))

(def width 320)
(def height 320)
(def test-image (atom nil))
(def partitions (atom nil))
(def image (i/load-image "./resources/test-image.jpg"))

(defn decider-fn
  [p1 p2]
  (let [vfn (fn [p] (c/rgba (.getRGB image (:x p) (:y p))))
        cie (c/cie76 (vfn p1) (vfn p2))]
    (< cie 20)))

(defn draw-cluster
  [cluster]
  (dorun
    (for [l cluster]
      (let []
        (quil/stroke-float 255 255 0)
        (quil/fill-float 255 255 0)
        (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l)))))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (quil/image @test-image 0 0)
  (dorun (draw-cluster (first partitions))))

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 10)
  (reset! test-image (quil/load-image  "/home/sanakan/code/mathematics/resources/test-image.jpg"))
  (reset! partitions (lf/partition (p/point 0 0) 319 319 decider-fn)))

(defn mouse-pressed [])
(defn mouse-released [])

(defn key-pressed [])

(defn show []
  (quil/sketch
    :title "line fill"
    :setup setup
    :draw draw
    :size [width height]
    ;:mouse-moved mouse-moved
    ;:mouse-dragged mouse-dragged
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-pressed key-pressed))

