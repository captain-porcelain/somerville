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
(def threshold-cie (atom 10))
(def threshold-cluster (atom 50))
(def image (i/load-image "./resources/test-image.jpg"))

(defn decider-fn
  [p1 p2]
  (let [vfn (fn [p] (c/rgba (.getRGB image (:x p) (:y p))))
        cie (c/cie76 (vfn p1) (vfn p2))]
    (< cie @threshold-cie)))

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
  (let [tmp (dorun (println "partitioning ..."))
        parts1 (lf/partition (p/point 0 0) 319 319 decider-fn)
        sizes (map lf/cluster-size parts1)
        avg1 (float (/ (reduce + sizes) (count sizes)))
        parts2 (filter #(< @threshold-cluster (lf/cluster-size %)) parts1)
        tmp (dorun (println (str "... done. found " (count parts1) " partitions with avg " avg1 " and filtered to " (count parts2))))]
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
      (reset! threshold-cie (+ @threshold-cie 1))
      (dorun (println (str "threshold-cie is " @threshold-cie)))))
  (if (= (quil/key-code) 45) ; -
    (let []
      (reset! threshold-cie (- @threshold-cie 1))
      (dorun (println (str "threshold-cie is " @threshold-cie)))))
  (if (= (quil/key-code) 80) ; p
    (dopartition))
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

