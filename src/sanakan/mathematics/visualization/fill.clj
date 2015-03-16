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
(def draw-fill (atom true))
(def threshold (atom 10))
(def image (i/load-image "./resources/test-image.jpg"))

(defn decider-fn
  [p1 p2]
  (let [vfn (fn [p] (c/rgba (.getRGB image (:x p) (:y p))))
        cie (c/cie76 (vfn p1) (vfn p2))]
    (< cie @threshold)))

(defn draw-cluster
  [cluster]
  (dorun
    (for [l cluster]
      (let [p (:p1 (first cluster))
            dc (c/rgba (.getRGB image (:x p) (:y p)))]
        (quil/stroke-float (:r dc) (:g dc) (:b dc))
        (quil/fill-float (:r dc) (:g dc) (:b dc))
        (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l)))))))

(defn dopartition
  []
  (let [parts (lf/partition (p/point 0 0) 319 319 decider-fn)
        parts2 (filter #(> 100 (lf/cluster-size %)) parts)]
    (reset! partitions parts2)))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (when (not @draw-fill) (quil/image @test-image 0 0))
  (when @draw-fill (dorun (for [cl @partitions] (draw-cluster cl)))))

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 1)
  (reset! test-image (quil/load-image  "/home/sanakan/code/mathematics/resources/test-image.jpg"))
  (dopartition))

(defn mouse-pressed [])
(defn mouse-released [])

(defn key-pressed []
  ;;(dorun (println (str "pressed code " (quil/key-code))))
  (if (= (quil/key-code) 521) ; +
    (let []
      (reset! threshold (+ @threshold 1))
      (dorun (println (str "threshold is " @threshold)))))
  (if (= (quil/key-code) 45) ; -
    (let []
      (reset! threshold (- @threshold 1))
      (dorun (println (str "threshold is " @threshold)))))
  (if (= (quil/key-code) 80) ; p 
    (let []
      (dorun (println "partitioning ..."))
      (dopartition)
      (dorun (println (str "... done. found " (count @partitions) " partitions")))))
  (if (= (quil/key-code) 68) ; d
    (reset! draw-fill (not @draw-fill))))

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

