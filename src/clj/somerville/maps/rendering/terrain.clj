(ns somerville.maps.rendering.terrain
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite RenderingHints Polygon BasicStroke]
    [java.awt.image BufferedImage])
  (:require
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.point :as p]
    [somerville.image :as image]
    [somerville.commons :as commons]
    [somerville.maps.grid :as grid]))


;;=======================================================================================================================
;; Calculating projections

(defn iso-projection
  [size x y]
  (p/point (* 0.5 (- (+ size x) y)) (* 0.5 (+ x y))))

(defn project
  [size width height x y z]
  (let [point (iso-projection size x y)
        x0    (* width 0.5)
        y0    (* height 0.2)
        x     (* 6 (- (:x point) (* size 0.5)))
        y     (* 1 (inc (* 0.005 (- size (:y point)))))
        z     (- (* size 3) (+ (* 0.4 z) (* (:y point) 0.05)))]
    (p/point (+ x0 (/ x y)) (+ y0 (/ z y)))))


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
    ;(cond
      ;(< b 0)        [0 0 0 128]
      ;(< b 128)      [b (* 2 b) b 255]
      ;(<= 128 b 220) [b b (/ b 3) 255]
      ;:else          [(min b 255) (min b 255) (min b 255) 255])))


;;=======================================================================================================================
;; Rendering the world as height blocks

(defn rect
  [graphics a b style]
  (when (> (:y b) (:y a))
    (.setPaint graphics (get-color style))
    (.fillRect graphics (:x a) (:y a) (- (:x b) (:x a)) (- (:y b) (:y a)))))

(defn new-image
  "Create a new image to hold the finished tiles."
  [config width height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        tmp (.setPaint graphics (get-color (:background-color config)))
        tmp (.fill graphics (Rectangle. 0 0 width height))]
    [img graphics]))

(def default-config
  {:background-color      [0 0 0 255]
   :water-color           [50 150 200 128]})

(defn draw
  [g config graphics width height]
  (let [size (:width g)
        water-val (* size 0.7)
        project   (partial project size width height)]
    (dotimes [y size]
      (dotimes [x size]
        (let [value  (get (grid/get-from g x y) :height 0)
              top    (project x y value)
              bottom (project (inc x) y 0)
              water  (project x y water-val)
              style  (brightness x y (- (get (grid/get-from g (inc x) y) :height 0) value))]
          (rect graphics top bottom style)
          (rect graphics water bottom (:water-color config)))))))

(defn render-height-blocks
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        tmp (draw g config graphics width height)
        tmp (.dispose graphics)]
    (image/write-image filename img)))

;;---------------------------------------------------------------------------------------------------------------
;; Rendering the world as triangulation

(defn draw-polygon
  "Draw a polygon onto the canvas."
  [graphics polygon line-color fill-color scale]
  (let [xs (into-array Integer/TYPE (map :x (polygon/to-points polygon)))
        ys (into-array Integer/TYPE (map :y (polygon/to-points polygon)))
        p (Polygon. xs ys (count xs))
        tmp (.setPaint graphics (get-color fill-color))
        tmp (.fillPolygon graphics p)
        tmp (.setPaint graphics (get-color line-color))
        tmp (.drawPolygon graphics p)]))

(defn create-triangles
  "Create a triangulation for the given grid."
  [g project-fn]
  (let [size (:width g)]
    (for [y (range size)
          x (range size)]
      (let [p1 [x y (get (grid/get-from g x y) :height 0)]
            p2 [(inc x) y (get (grid/get-from g (inc x) y) :height 0)]
            p3 [(inc x) (inc y) (get (grid/get-from g (inc x) (inc y)) :height 0)]
            p4 [x (inc y) (get (grid/get-from g x (inc y)) :height 0)]
            t1 (polygon/from-points (list (project-fn p1) (project-fn p2) (project-fn p4)))
            t2 (polygon/from-points (list (project-fn p2) (project-fn p3) (project-fn p4)))]
        [t1 t2]))))

(defn polygon-max
  [p k]
  (apply max (map k (polygon/to-points p))))

(defn pair-max
  [p k]
  (apply max (map #(polygon-max % k) p)))

(defn found-max
  [p k]
  (apply max (map #(pair-max % k) p)))

(defn scale-factor
  [triangles width]
  (/ (- width 20) (found-max triangles :x)))

(defn draw-triangles
  "Draw pairs of triangles."
  [g config graphics width height]
  (let [size (:width g)
        pject (fn [v] (project size width height (nth v 0) (nth v 1) (nth v 2)))
        triangles (create-triangles g pject)
        tmp (dorun (println (str "Found max: " (found-max triangles :x) ", scale: " (scale-factor triangles width))))
        scale (scale-factor triangles width)]
    (dorun (map #(let [[t1 t2] %]
                   (draw-polygon graphics t1 [30 30 30 255] [128 20 128 255] scale)
                   (draw-polygon graphics t2 [30 30 30 255] [80 10 80 255] scale))
                triangles))))

(defn render-triangulation
  "Render the grid using a triangulation."
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        tmp (draw-triangles g config graphics width height)
        tmp (.dispose graphics)]
    (image/write-image filename img)))

