(ns somerville.maps.terrain.rendering.terrain
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite RenderingHints Polygon BasicStroke]
    [java.awt.image BufferedImage])
  (:require
    [somerville.maps.terrain.rendering.conrec :as conrec]
    [somerville.geometry.projection :as projection]
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.image :as image]
    [somerville.commons :as commons]
    [somerville.geometry.commons :as c]
    [somerville.maps.grid :as grid]))

;;=======================================================================================================================
;; Calculating colors

(defn get-color
  "Convert integer array into Color."
  [c]
  (Color. ^Integer (int (nth c 0)) ^Integer (int (nth c 1)) ^Integer (int (nth c 2)) ^Integer (int (nth c 3))))

(defn minmax
  [v]
  (max 0 (min v 255)))

(defn brightness
  [x y slope]
  (let [b (+ 128 (Math/floor (* slope 50)))
        mb (minmax b)]
    [mb mb mb 255]))


;;=======================================================================================================================
;; General Rendering

(defn new-image
  "Create a new image to hold the finished tiles."
  [config width height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        tmp (.setPaint graphics (get-color (:background-color config)))
        tmp (.fill graphics (Rectangle. 0 0 width height))]
    [img graphics]))

(defn draw-polygon
  "Draw a polygon onto the canvas."
  [graphics polygon line-color fill-color]
  (let [xs (into-array Integer/TYPE (map :x (polygon/to-points polygon)))
        ys (into-array Integer/TYPE (map :y (polygon/to-points polygon)))
        p (Polygon. xs ys (count xs))
        ;tmp (.setPaint graphics (get-color fill-color))
        ;tmp (.fillPolygon graphics p)
        tmp (.setPaint graphics (get-color line-color))
        tmp (.drawPolygon graphics p)]))

(defn render-line
  [graphics line line-color]
  ;(.fillOval graphics (- (:x (:p1 line)) 2) (- (:y (:p1 line)) 4) 4 4)
  ;(.fillOval graphics (- (:x (:p2 line)) 2) (- (:y (:p2 line)) 2) 4 4)
  (.setPaint graphics (get-color line-color))
  (.drawLine graphics (:x (:p1 line)) (:y (:p1 line)) (:x (:p2 line)) (:y (:p2 line))))

(def default-config
  {:background-color      [0 0 0 255]
   :line-color            [128 20 128 255]
   :water-color           [50 150 200 128]
   :height-steps          2})


;;=======================================================================================================================
;; Rendering the world as triangulation

(defn triangulation
  [g]
  (let [size (dec (:width g))]
    (for [x (range size)
          y (range size)
          which [:upper :lower]]
      (let [p1 (p/point x y (grid/get-from g x y))
            p2 (p/point (inc x) y (grid/get-from g (inc x) y))
            p3 (p/point (inc x) (inc y) (grid/get-from g (inc x) (inc y)))
            p4 (p/point x (inc y) (grid/get-from g x (inc y)))]
        (if (= :upper which)
          (polygon/from-points (list p1 p2 p4))
          (polygon/from-points (list p2 p3 p4)))))))

(defn triangulation-4
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
        triangles (triangulation-4 g)
        projected (map pject triangles)]
    (dorun (map #(draw-polygon graphics % [128 20 128 255] [128 20 128 255]) projected))))

(defn render-triangulation
  "Render the grid using a triangulation."
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        tmp (draw-triangles g config graphics width height)
        tmp (.dispose graphics)]
    (image/write-image filename img)))


;;=======================================================================================================================
;; Rendering the world as height lines

(defn draw-height-lines
  [graphics config scale lines]
  (dorun (map #(render-line graphics (l/scale % scale) (:line-color config)) lines)))

(defn grid-lines
  [g height]
  (concat
    (for [x (range (inc (:width g)))] (l/line (p/point x 0 height) (p/point x (:height g) height)))
    (for [y (range (inc (:height g)))] (l/line (p/point 0 y height) (p/point (:width g) y height)))))

(defn render-flat-heights
  "Render height lines as seen from above."
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        contour (conrec/contour g (:height-steps config))
        scale (/ width (:width g))
        tmp (dorun (map #(render-line graphics (l/scale % scale) [10 128 55 255]) (grid-lines g 0)))
        tmp (dorun (map #(draw-height-lines graphics config scale %) (map :lines contour)))
        tmp (.dispose graphics)]
    (image/write-image filename img)))


(defn draw-heights
  "Draw height lines."
  [g contour config graphics width height]
  (let [scale 20
        camera (p/point (* -10 (/ (:width g) 2)) (* -10 (/ (:height g) 2)) 30)
        focus  (p/point (* (:width g) 15) (* (:height g) 15)  0)
        up     (p/cross (p/subtract focus camera) (p/subtract (p/point (:width g) 0 0) (p/point 0 (:height g) 0)))
        projector (projection/projector camera focus up 10 width height)
        scale (/ width (:width g))
        scale 8
        pject (fn [v] (projection/project projector (p/point (* scale (:x v)) (* scale (:y v)) (:z v))))
        tmp (render-line graphics (l/line (pject (p/point 0 0 0)) (pject (p/point 0 0 18))) [10 128 55 255])
        tmp (dorun (map #(render-line graphics (l/line (pject (:p1 %)) (pject (:p2 %))) [10 128 55 255]) (grid-lines g 0)))
        tmp (dorun (map #(render-line graphics (l/line (pject (:p1 %)) (pject (:p2 %))) [128 10 55 255]) (grid-lines g 7)))
        tmp (dorun (map #(render-line graphics (l/line (pject (:p1 %)) (pject (:p2 %))) [10 55 128 255]) (grid-lines g 18)))
        h3d-lines (map
                    #(map
                       (fn [line]
                         (l/line
                           (p/point (:x (:p1 line)) (:y (:p1 line)) (:height %))
                           (p/point (:x (:p2 line)) (:y (:p2 line)) (:height %))))
                       (:lines %))
                    contour)]
    (dorun
      (map
        #(dorun
           (map
             (fn [line]
               (render-line graphics (l/line (pject (:p1 line)) (pject (:p2 line))) (:line-color config)))
             %))
        h3d-lines))))

(defn render-heights
  "Render height lines in 3d."
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        contour (conrec/contour g (:height-steps config))
        tmp (draw-heights g contour config graphics width height)
        tmp (.dispose graphics)]
    (image/write-image filename img)))

