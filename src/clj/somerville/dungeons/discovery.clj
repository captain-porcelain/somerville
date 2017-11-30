;; Functions to create overlay images that only show parts of an image that have been discovered.
(ns somerville.dungeons.discovery
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite]
    [java.awt.image BufferedImage])
  (:require
    [somerville.commons :as commons]
    [somerville.image :as image]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.rasterize :as rasterize]
    [taoensso.timbre :as log]))

;;==================================================================================================================
;; Parsing wall description

(defn translate-line
  "Translate a line description into an actual line."
  [parameters]
  ;; Check that we have two parameters
  (when (= 2 (count parameters))
    (let [parts (reduce concat (map #(clojure.string/split % #",") parameters))
          iparts (filter #(not (nil? %)) (map #(commons/parse-int % nil) parts))]
      (when (= 4 (count iparts))
        (l/line (p/point (nth iparts 0) (nth iparts 1)) (p/point (nth iparts 2) (nth iparts 3)))))))

(defn translate-description
  "Translate one wall description."
  [^String description]
  (let [parts (clojure.string/split (.trim description) #"\s+")]
    (when (< 1 (count parts))
      (case (first parts)
        "line" (translate-line (rest parts))
        :else nil))))

(defn parse
  "Given a string representation of wall descriptions create the geometrical representations for them."
  [^String wall-description]
  (filter #(not (nil? %)) (map translate-description (clojure.string/split wall-description #"\n"))))


;;==================================================================================================================
;; Geometric discovery
;;

(defn relevant-lines
  "Filter given lines to those that intersect the circle or are contained by it."
  [circle lines]
  (filter #(or
             (c/point-in? circle (:p1 %))
             (c/point-in? circle (:p2 %))
             (< 0 (count (c/intersect-line-segment circle %))))
          lines))

(defn create-undiscovered-graphics
  "Create an image of size width x height with transparency and paint it completely black."
  [^Integer width ^Integer height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics Color/black)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.dispose graphics)]
    img))

(defn discover-circle
  "Given a point of origin, check all pixels on the circle given by this origin for
  intersections of the line from origin to the pixel with any wall lines.
  Find the outermost visible pixel and consider the line from the center to the pixel as visible."
  [wall-lines ^Graphics2D graphics visualrange origin circle-base-points]
  (let [center (p/point (nth origin 0) (nth origin 1))
        circle (c/circle center visualrange)
        relevant-walls (relevant-lines circle wall-lines)
        circle-points (rasterize/translate-line circle-base-points origin)]
    (dorun
      (for [[x y] circle-points]
        (let [point (p/point x y)
              center-line (l/line center point)
              intersections (filter #(not (nil? %)) (map #(l/intersect-segments center-line %) relevant-walls))
              nearest (first (sort-by #(p/distance % center) intersections))
              visible-to (if (nil? nearest) point nearest)
              tmp (.drawLine graphics (:x visible-to) (:y visible-to) (nth origin 0) (nth origin 1))]
          graphics)))))

(defn discover-circles
  "Given a point of origin, check all pixels on the circle given by this origin for
  intersections of the line from origin to the pixel with any wall lines.
  Find the outermost visible pixel and consider the line from the center to the pixel as visible."
  [points wall-lines ^BufferedImage discovered-image visualrange]
  (let [circle-base-points (rasterize/circle visualrange)
        graphics ^Graphics2D (.createGraphics discovered-image)
        tmp (.setPaint graphics (Color. 255 255 255 0))
        tmp (.setComposite graphics AlphaComposite/Clear)
        tmp (dorun (map #(discover-circle wall-lines graphics visualrange % circle-base-points) points))
        tmp (.dispose graphics)]
    discovered-image))

(defn discover
  "Create a black image that is set to transparent in areas that are visible from discovered points.
  Based on the amount of pixels decide upon the strategy. Either using bounding boxes for the circles
  around the points or just check all pixels of the image."
  [points wall-description width height visualrange]
  (let [discovered-image (create-undiscovered-graphics width height)
        wall-lines (parse wall-description)
        tmp (do (discover-circles points wall-lines discovered-image visualrange))]
    discovered-image))
