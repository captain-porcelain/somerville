(ns somerville.artsy.image-triangulation
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite]
    [java.awt.image BufferedImage])
  (:require
    [somerville.fills.flood-fill :as ff]
    [somerville.image :as i]
    [somerville.geometry.point :as p]
    [somerville.fills.convex-hull :as cxh]
    [somerville.fills.concave-hull :as cvh]
    [somerville.color.color :as c]
    [somerville.geometry.rendering.svg :as svg]
    [clojure.string :as s]))

;;==================================================================================================================
;; Image Processing

(defn make-decider-fn
  "Create a decider function for the flood fill. It is based on color similarity."
  [image threshold-cie]
  (fn [p1 p2]
    (let [vfn (fn [p] (c/rgba (.getRGB image (p/x p) (p/y p))))
          cie (c/cie76 (vfn p1) (vfn p2))]
      (< cie threshold-cie))))

(defn add-seed-color
  "Add color of seed point to partition."
  [part image]
  (assoc part :color (.getRGB image (p/x (:seed part)) (p/y (:seed part)))))

(defn add-seed-colors
  "Add color of seed point to all partitions."
  [partitions image]
  (map #(add-seed-color % image) partitions))

(defn partition-center
  "Find center point of partition."
  [points]
  (p/point
    (int (/ (reduce + (map p/x points)) (count points)))
    (int (/ (reduce + (map p/y points)) (count points)))))

(defn add-center
  "Add center point to partition."
  [part]
  (assoc part :center (partition-center (:points part))))

(defn add-centers
  "Add center point to all partitions."
  [partitions]
  (map add-center partitions))

(defn find-partitions
  [image samples sensitivity]
  (let [decider-fn (make-decider-fn image sensitivity)]
    (add-centers (add-seed-colors (ff/partition-samples samples decider-fn 0 0 (.getWidth image) (.getHeight image)) image))))

;;==================================================================================================================
;; Debugging helpers

(defn draw-partition
  [image part]
  (dorun (map #(.setRGB image (p/x %) (p/y %) (:color part)) (:points part))))

(defn draw-partitions
  [filename width height partitions]
  (let [image (i/make-image width height)
        tmp (dorun (map #(draw-partition image %) partitions))]
    (i/write-image filename image)))

(defn draw-partitions-separately
  [filename width height partitions]
  (dorun
    (map #(let [filename (str (s/replace filename ".png" "-") (p/x (:seed %)) "-" (p/y (:seed %)) "-pixel.png")
                image (i/make-image width height)
                tmp (draw-partition image %)]
            (i/write-image filename image)) partitions)))

(defn render-hull
  "Render grid walls to image."
  [filename width height polygon seed]
  (let [filename (str (s/replace filename ".png" "-") (p/x seed) "-" (p/y seed) "-hull.png")
        image (i/make-image width height)
        graphics ^Graphics2D (.createGraphics image)
        tmp (.setPaint graphics Color/white)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.setPaint graphics Color/black)
        tmp (dorun (map #(.drawLine graphics (p/x (:p1 %)) (p/y (:p1 %)) (p/x (:p2 %)) (p/y (:p2 %))) (:lines polygon)))
        tmp (.dispose graphics)]
    (i/write-image filename image)))

(defn render-hulls
  [filename width height hulls]
  (dorun (map #(render-hull filename width height (:hull %) (:seed %)) hulls)))


;;==================================================================================================================
;; Actual triangulation

(defn triangulate-image
  "Create a voronoi representation of an image."
  [image-file-name]
  (let [image (i/load-image image-file-name)
        partitions (find-partitions image 1000 15)
        tmp (dorun (println (str "Found " (count partitions) " partitions")))
        ;approximation1 (map #(assoc % :hull (cxh/quick-hull (:points %))) partitions)
        ;approximation2 (map #(assoc % :hull (cvh/hull (:points %) 20)) partitions)
        tmp (dorun (map #(println (count (:points %))) partitions))
        tmp (draw-partitions-separately "/tmp/partitions.png" (.getWidth image) (.getHeight image) partitions)
        ;tmp (draw-partitions "/tmp/partitions.png" (.getWidth image) (.getHeight image) partitions)
        ;tmp (render-hulls "/tmp/partitions.png" (.getWidth image) (.getHeight image) approximation2)
        ]
    ;(svg/voronoi approximation "/tmp/voronoi.svg")
    partitions
    ))

