;; Provides the facilities to manipulate images
(ns somerville.dungeons.discovery.pixel
  (:import
    [java.awt Color Graphics2D Rectangle]
    [java.awt.image BufferedImage]
    [javax.imageio ImageIO]
    [java.io File])
  (:require
    [clojure.java.io :as io]
    [taoensso.timbre :as log]
    [somerville.commons :as commons]
    [somerville.image :as image]
    [somerville.dungeons.discovery.rasterize :as rasterize]))

(def black (.getRGB (Color. 0 0 0 0)))
(def transparent (.getRGB (Color. 0 0 0 1)))

(defn filter-line
  "Filter a line to those pixels that represent free space."
  [^BufferedImage img l]
  (take-while #(image/free? img %) l))

(defn filter-all
  "User filter-line to filter a list lines to those pixels that represent free space."
  [lines wall]
  (map #(filter-line wall %) lines))

(defn update-discovered
  "Update an image and set the pixels in the sightlines to be discovered."
  [^BufferedImage img sightlines]
  (dorun
    (for [line sightlines]
      (dorun
        (for [[x y] line]
          (when-not (image/free? img [x y])
            (.setRGB img x y (.getRGB (Color. 0 0 0 1)))))))))

(defn update-list-discovered
  "Use update-discovered for a list of list of sightlines."
  [lines ^BufferedImage img]
  (loop [l lines]
    (if (= 0 (count l))
      img
      (let [t (update-discovered img (first l))]
        (recur (rest l))))))

(defn start-end
  [l]
  [(first (first l)) (second (first l)) (first (last l)) (second (last l))])

(defn draw-line
  [graphics l]
  (let [[x1 y1 x2 y2] (start-end l)]
    (when-not (or (nil? x1) (nil? y1) (nil? x2) (nil? y2))
      (.drawLine graphics x1 y1 x2 y2))))

(defn draw-discovery
  [graphics d]
  (dorun (map #(draw-line graphics %) d)))

(defn update-list-discovered-line
  "Use update-discovered for a list of list of sightlines."
  [lines ^BufferedImage img]
  (let [c (Color. 0 0 0 1)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics c)
        tmp (dorun (map #(draw-discovery graphics %) lines))
        tmp (.dispose graphics)]
    img))

(defn create-undiscovered
  "Create an image of size width x height with transparency and paint it completely black."
  [^Integer width ^Integer height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        tmp (dorun
              (for [x (range width)
                    y (range height)]
                (.setRGB img x y (.getRGB Color/black))))]
    img))

(defn create-undiscovered-graphics
  "Create an image of size width x height with transparency and paint it completely black."
  [^Integer width ^Integer height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics Color/black)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.dispose graphics)]
    img))

(defn discover-list
  "Load a wallmap and create a fresh discovered image. For each point given the discovered image is updated."
  [wallmap points visualrange]
  (let [wall ^BufferedImage (image/load-image wallmap)
        width (.getWidth wall)
        height (.getHeight wall)
        img (create-undiscovered-graphics width height)
        ;img (create-undiscovered width height)
        lines (rasterize/sight-lines visualrange)
        sightlines (map #(rasterize/translate-lines lines %) points)
        filteredlines (map #(filter-all % wall) sightlines)
        tmp (dorun (println (str "Total: " (reduce + (map #(reduce + (map count %)) filteredlines)))))
        tmp (update-list-discovered-line filteredlines img)]
        ;tmp (update-list-discovered filteredlines img)]
    img))
