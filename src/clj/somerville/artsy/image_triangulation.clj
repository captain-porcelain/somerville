(ns somerville.artsy.image-triangulation
  (:require
    [somerville.fills.flood-fill :as ff]
    [somerville.image :as i]
    [somerville.geometry.point :as p]
    [somerville.geometry.voronoi :as v]
    [somerville.color.color :as c]
    [somerville.geometry.rendering.svg :as svg]))

(defn make-decider-fn
  "Create a decider function for the flood fill. It is based on color similarity."
  [image threshold-cie]
  (fn [p1 p2]
    (let [vfn (fn [p] (c/rgba (.getRGB image (:x p) (:y p))))
          cie (c/cie76 (vfn p1) (vfn p2))]
      (< cie threshold-cie))))

(defn partition-center
  "Find center point of partition."
  [part]
  (p/point
    (int (/ (reduce + (map :x part)) (count part)))
    (int (/ (reduce + (map :y part)) (count part)))))

(defn triangulate-image
  "Create a voronoi representation of an image."
  [image-file-name]
  (let [image (i/load-image image-file-name)
        decider-fn (make-decider-fn image 10)
        partitions (ff/partition-samples 1000 decider-fn 0 0 (.getWidth image) (.getHeight image))
        tmp (dorun (println (str "Found " (count partitions) " partitions")))
        centers (map partition-center partitions)
        approximation (v/voronoi centers 0 0 (.getWidth image) (.getHeight image))
        tmp (dorun (println (str "Writing SVG")))]
    (svg/voronoi approximation "/tmp/voronoi.svg")))

