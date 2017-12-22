;; Geometric discovery to create overlay images that only show parts of an image that have been discovered.
;; The discovery is based on ray casting to wall points.
;; See https://www.redblobgames.com/articles/visibility/
(ns somerville.dungeons.discovery.image
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite Polygon BasicStroke RenderingHints]
    [java.awt.image BufferedImage])
  (:require
    [somerville.commons :as commons]
    [somerville.dungeons.discovery.parser :as parser]
    [somerville.dungeons.discovery.ray-cast-wall-trace :as rcwt]
    [somerville.geometry.commons :as gcommons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.circle :as c]
    [somerville.geometry.triangle :as t]
    [somerville.geometry.polygon :as poly]
    [taoensso.timbre :as log]))

(defn create-undiscovered-graphics
  "Create an image of size width x height with transparency and paint it completely black."
  [^Integer width ^Integer height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics Color/black)
        tmp (.fill graphics (Rectangle. 0 0 width height))
        tmp (.dispose graphics)]
    img))

(defn setup-graphics
  "Prepare the Java Graphics object for rendering."
  [image]
  (let [graphics ^Graphics2D (.createGraphics image)
        tmp (.setPaint graphics (Color. 255 255 255 0))
        tmp (.setComposite graphics AlphaComposite/Clear)
        tmp (.setStroke graphics (BasicStroke. 2))
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)]
  graphics))

(defn draw-triangle
  "Transform a triangle into a Java graphics polygon and render it."
  [triangle graphics]
  (let [xs (into-array Integer/TYPE (list (:x (:p1 triangle)) (:x (:p2 triangle)) (:x (:p3 triangle))))
        ys (into-array Integer/TYPE (list (:y (:p1 triangle)) (:y (:p2 triangle)) (:y (:p3 triangle))))
        p (Polygon. xs ys (count xs))
        tmp (.fillPolygon graphics p)
        tmp (.drawPolygon graphics p)]))

(defn discover-all-points
  "Discover the visible areas based on ray casting with wall tracing."
  [points wall-lines ^BufferedImage graphics visualrange]
  (let [ps (map #(p/point (nth % 0) (nth % 1)) points)
        triangles (reduce concat (map #(rcwt/discover-point % wall-lines visualrange) ps))]
    (dorun (map #(draw-triangle % graphics) triangles))))

(defn discover
  "Create a black image that is set to transparent in areas that are visible from discovered points.
  Based on the amount of pixels decide upon the strategy. Either using bounding boxes for the circles
  around the points or just check all pixels of the image."
  [points wall-description width height visualrange]
  (let [discovered-image (create-undiscovered-graphics width height)
        graphics ^Graphics2D (setup-graphics discovered-image)
        wall-lines (parser/parse wall-description)
        tmp (when @rcwt/debug (reset! rcwt/debug-fn-1 (rcwt/make-debug-fn-1 width height visualrange)))
        tmp (do (discover-all-points points wall-lines graphics visualrange))
        tmp (.dispose graphics)]
    discovered-image))
