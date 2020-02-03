;; See http://paulbourke.net/papers/conrec/
(ns somerville.rendering.terrain.conrec
  (:import
    [java.awt Graphics2D Rectangle RenderingHints]
    [java.awt.image BufferedImage])
  (:require
    [somerville.color.color :as color]
    [somerville.rendering.image :as image]
    [somerville.geometry.line :as l]
    [somerville.rasterization.conrec :as conrec]))


;;=======================================================================================================================
;; Rendering the height lines

(defn new-image
  "Create a new image to hold the finished tiles."
  [config width height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        tmp (.setPaint graphics (image/to-awt (apply color/rgba (:background-color config))))
        tmp (.fill graphics (Rectangle. 0 0 width height))]
    [img graphics]))

(defn render-line
  [graphics line line-color width height]
  (.setPaint graphics (image/to-awt (apply color/rgba line-color)))
  (.drawLine graphics (- width (:x (:p1 line))) (- height (:y (:p1 line))) (- width (:x (:p2 line))) (- height (:y (:p2 line)))))

(defn draw-height-lines
  [graphics c scale lines width height]
  (dorun (map #(render-line graphics (l/scale % scale) c width height) lines)))

(defn get-colors
  [config]
  (if (nil? (:colors config))
    (let [c1 (apply color/rgba (:line-color-1 config))
          c2 (apply color/rgba (:line-color-2 config))]
      (map color/to-vector (color/color-steps c1 c2 (:height-steps config))))
    (:colors config)))

(def default-config
  {:background-color      [0 0 0 255]
   :line-color-1          [128 20 128 255]
   :line-color-2          [255 255 255 255]
   :heights               [-1000 10 20 28 32]
   :colors                [[0 161 228 255] [204 88 3 255] [137 252 0 255] [160 160 160 255] [240 240 240 255]]
   :height-steps          4})

(defn render
  "Render height lines as seen from above."
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        contour (if (nil? (:heights config)) (conrec/contour g (:height-steps config)) (conrec/contour-preselected g (:heights config)))
        scale (/ width (:width g))
        tmp (dorun (map #(draw-height-lines graphics %2 scale %1 width height) (map :lines contour) (get-colors config)))
        tmp (.dispose graphics)]
    (image/write-image filename img)))

