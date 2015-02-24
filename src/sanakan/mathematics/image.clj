;; Provides the facilities to manipulate images
(ns sanakan.mathematics.image
  (:import java.awt.image.BufferedImage)
  (:import java.awt.Color)
  (:import javax.imageio.ImageIO)
  (:import java.io.File))

;; try to improve performance by having hints when reflection is needed.
(set! *warn-on-reflection* true)

(defn load-image
  "Load an image file containing a map from disc."
  [^String filename]
  (ImageIO/read (File. filename)))

(defn size
  "Get size of image from disc."
  [^String filename]
  (let [i (load-image filename)]
    {:width (.getWidth i) :height (.getHeight i)}))

(defn write-image
  "Write an image file containing a map to disc."
  [^String filename ^BufferedImage img]
  (ImageIO/write img "png" (File. filename)))

(defn in-bounds?
  "Check if a pixel is inside an image."
  [^BufferedImage img [^Integer x ^Integer y]]
  (and
    (< x (.getWidth img))
    (> x -1)
    (< y (.getHeight img))
    (> y -1)))

(defn free?
  "Check if a pixel represents a free space."
  [^BufferedImage img [^Integer x ^Integer y]]
  (if (in-bounds? img [x y])
    (= Color/WHITE (Color. (.getRGB img x y)))
    false))

(defn update-discovered
  "Update an image and set the pixels in the sightlines to be discovered."
  [^BufferedImage img sightlines]
  (dorun
    (for [line sightlines]
      (dorun
        (for [[x y] line]
          (.setRGB img x y (.getRGB (Color. 0 0 0 1))))))))


