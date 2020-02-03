(ns somerville.rendering.terrain.terrain
  (:import
    [java.awt Color Graphics2D Rectangle RenderingHints Polygon]
    [java.awt.image BufferedImage])
  (:require
    [somerville.rasterization.conrec :as conrec]
    [somerville.geometry.projection.projection :as projection]
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.color.color :as color]
    [somerville.rendering.image :as image]))


;;=======================================================================================================================
;; General Rendering

(defn new-image
  "Create a new image to hold the finished tiles."
  [config width height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        tmp (.setPaint graphics (image/to-awt (apply color/rgba (:background-color config))))
        tmp (.fill graphics (Rectangle. 0 0 width height))]
    [img graphics]))

(defn draw-polygon
  "Draw a polygon onto the canvas."
  [graphics polygon line-color fill-color]
  (let [xs (into-array Integer/TYPE (map :x (polygon/to-points polygon)))
        ys (into-array Integer/TYPE (map :y (polygon/to-points polygon)))
        p (Polygon. xs ys (count xs))
        ;tmp (.setPaint graphics (image/to-awt (apply color/rgba fill-color)))
        ;tmp (.fillPolygon graphics p)
        tmp (.setPaint graphics (image/to-awt (apply color/rgba line-color)))
        tmp (.drawPolygon graphics p)]))

(defn render-line
  [graphics line line-color]
  ;(.fillOval graphics (- (:x (:p1 line)) 2) (- (:y (:p1 line)) 4) 4 4)
  ;(.fillOval graphics (- (:x (:p2 line)) 2) (- (:y (:p2 line)) 2) 4 4)
  (.setPaint graphics (image/to-awt (apply color/rgba line-color)))
  (.drawLine graphics (:x (:p1 line)) (:y (:p1 line)) (:x (:p2 line)) (:y (:p2 line))))

(def default-config
  {:background-color      [0 0 0 255]
   :line-color            [128 20 128 255]
   :water-color           [50 150 200 128]
   :height-steps          4})


;;=======================================================================================================================
;; Rendering the world as triangulation

(defn triangulation
  [g]
  (let [size (dec (dec (:width g)))]
    (for [x (range size)
          y (range size)
          i (range 4)]
      (let [heights (conrec/triangle-heights g x y)]
        (case i
          0 (polygon/from-points (list (:p1 heights) (:p0 heights) (:p2 heights)))
          1 (polygon/from-points (list (:p2 heights) (:p0 heights) (:p3 heights)))
          2 (polygon/from-points (list (:p3 heights) (:p0 heights) (:p4 heights)))
          3 (polygon/from-points (list (:p4 heights) (:p0 heights) (:p1 heights))))))))

(defn draw-triangles
  "Draw pairs of triangles."
  [g config graphics width height]
  (let [camera (p/point (/ (:width g) 2) 0 20)
        focus  (p/point (/ (:width g) 2) (/ (:height g) 2)  0)
        up     (p/cross (p/subtract focus camera) (p/subtract (p/point (:width g) 0 0) (p/point 0 (:height g) 0)))
        projector (projection/projector camera focus up 2 width height)
        pject (fn [t] (polygon/from-points (map #(projection/project projector %) (polygon/to-points t))))
        triangles (triangulation g)
        projected (map pject triangles)]
    (dorun (map #(draw-polygon graphics % [128 20 128 255] [128 20 128 255]) projected))))

(defn render-triangulation
  "Render the grid using a triangulation."
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        tmp (draw-triangles g config graphics width height)
        tmp (.dispose graphics)]
    (image/write-image filename img)))

