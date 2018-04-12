(ns somerville.maps.terrain.cartographer
  (:import
    [java.awt.image BufferedImage])
  (:require
    [somerville.image :as image]
    [somerville.color.color :as color]))

(defn find-edges
  "Assume filename points to an image where a coastline is identified by pixels that are not transparent.
  Returns the list of pixels with a height of 1."
  [filename]
  (let [ifile (image/load-image filename)
        width (.getWidth ifile)
        height (.getHeight ifile)]
    (filter
      #(not (nil? %))
      (for [x (range width)
            y (range height)]
        (let [c (color/rgba (.getRGB ifile x y))]
          (when (< 0 (:a c)) [x y {:level :flat}]))))))

(defn parse-mask
  "Assume filename points to an image with a mask:
  - blue pixels mean water and are restricted to a level below 0
  - green pixels mean land mass and are restricted to a level above 0
  Returns the list of pixels with restrictions."
  [filename]
  (let [ifile (image/load-image filename)
        width (.getWidth ifile)
        height (.getHeight ifile)
        blue (color/rgba 0 0 255 255)
        green (color/rgba 0 255 0 255)]
    (filter
      #(not (nil? %))
      (for [x (range width)
            y (range height)]
        (let [c (color/rgba (.getRGB ifile x y))]
          (cond
            (color/same? blue c) [x y {:level :below}]
            (color/same? green c) [x y {:level :above}]
            :else [x y c blue (color/same? blue c)]))))))
