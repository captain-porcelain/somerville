;; Provides the facilities to manipulate images
(ns somerville.image
  (:import
    [java.awt Color]
    [java.awt.image BufferedImage]
    [javax.imageio ImageIO]
    [java.io File])
  (:require
    [somerville.geometry.point :as p]
    [clojure.java.io :as io]))

;; try to improve performance by having hints when reflection is needed.
(set! *warn-on-reflection* true)

(defn load-image
  "Load an image file containing a map from disc."
  [^String filename]
  (ImageIO/read (File. filename)))

(defn load-image-resource
  "Load an image file containing a map from jar resources."
  [^String filename]
  (ImageIO/read (io/resource filename)))

(defn size
  "Get size of image from disc."
  [^String filename]
  (let [i ^BufferedImage (load-image filename)]
    {:width (.getWidth i) :height (.getHeight i)}))

(defn size-resource
  "Get size of image from jar resources."
  [^String filename]
  (let [i ^BufferedImage (load-image-resource filename)]
    {:width (.getWidth i) :height (.getHeight i)}))

(defn make-image
  "Create new BufferedImage of given size."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))

(defn write-image
  "Write an image file containing a map to disc."
  [^String filename ^BufferedImage img]
  (ImageIO/write img "png" (File. filename)))

(defn in-bounds-raw?
  "Check if a pixel is inside an image."
  [^BufferedImage img [^Integer x ^Integer y]]
  (and
    (< x (.getWidth img))
    (> x -1)
    (< y (.getHeight img))
    (> y -1)))

(defn in-bounds?
  "Check if a pixel is inside an image."
  [^BufferedImage img p]
  (in-bounds-raw? img [(p/x p) (p/y p)]))

(defn free?
  "Check if a pixel represents a free space."
  [^BufferedImage img [^Integer x ^Integer y]]
  (if (in-bounds-raw? img [x y])
    (= Color/WHITE (Color. (.getRGB img x y)))
    false))

