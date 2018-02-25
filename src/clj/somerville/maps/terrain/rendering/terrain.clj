(ns somerville.maps.terrain.rendering.terrain
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite RenderingHints Polygon BasicStroke]
    [java.awt.image BufferedImage])
  (:require
    [somerville.geometry.projection :as projection]
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.point :as p]
    [somerville.image :as image]
    [somerville.commons :as commons]
    [somerville.geometry.commons :as c]
    [somerville.maps.grid :as grid]
    ))

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
        project   (partial projection/project size width height)]
    (dotimes [y size]
      (dotimes [x size]
        (let [value  (grid/get-from g x y)
              top    (project x y value)
              bottom (project (inc x) y 0)
              water  (project x y water-val)
              next-val (grid/get-from g (inc x) y)
              style  (brightness x y (- (if (int? next-val) next-val 0) value))]
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
  [graphics polygon line-color fill-color]
  (let [xs (into-array Integer/TYPE (map :x (polygon/to-points polygon)))
        ys (into-array Integer/TYPE (map :y (polygon/to-points polygon)))
        p (Polygon. xs ys (count xs))
        ;tmp (.setPaint graphics (get-color fill-color))
        ;tmp (.fillPolygon graphics p)
        tmp (.setPaint graphics (get-color line-color))
        tmp (.drawPolygon graphics p)]))

(defn grid-points
  [g]
  (let [size (dec (:width g))]
    (for [y (range size)
          x (range size)]
      [x y])))

(defn triangles
  [g [x y]]
  (let [p1 (p/point x y (grid/get-from g x y))
        p2 (p/point (inc x) y (grid/get-from g (inc x) y))
        p3 (p/point (inc x) (inc y) (grid/get-from g (inc x) (inc y)))
        p4 (p/point x (inc y) (grid/get-from g x (inc y)))
        t1 (polygon/from-points (list p1 p2 p4))
        t2 (polygon/from-points (list p2 p3 p4))]
    [t1 t2]))

(defn triangulation
  [g]
  (loop [p2s (grid-points g)
         p3s (list)]
    (if (= 0 (count p2s))
      p3s
      (recur (rest p2s) (concat p3s (triangles g (first p2s)))))))

(defn create-triangles
  "Create a triangulation for the given grid."
  [g project-fn]
  (let [size (dec (:width g))]
    (for [y (range size)
          x (range size)]
      (let [p1 [x y (grid/get-from g x y)]
            p2 [(inc x) y (grid/get-from g (inc x) y)]
            p3 [(inc x) (inc y) (grid/get-from g (inc x) (inc y))]
            p4 [x (inc y) (grid/get-from g x (inc y))]
            t1 (polygon/from-points (list (project-fn p1) (project-fn p2) (project-fn p4)))
            t2 (polygon/from-points (list (project-fn p2) (project-fn p3) (project-fn p4)))]
        [t1 t2]))))

(defn polygon-min-max
  [p k f]
  (apply f (map k (polygon/to-points p))))

(defn pair-min-max
  [p k f]
  (apply f (map #(polygon-min-max % k f) p)))

(defn found-max
  [p k]
  (apply max (map #(pair-min-max % k max) p)))

(defn found-min
  [p k]
  (apply min (map #(pair-min-max % k min) p)))

(defn scale-factor
  [triangles width offset]
  (/ (+ width offset) (found-max triangles :x)))

(defn draw-triangles
  "Draw pairs of triangles."
  [g config graphics width height]
  (let [scale 20
        camera (p/point (* -20 (/ (:width g) 2)) (* -20 (/ (:height g) 2)) 100)
        focus  (p/point (/ (:width g) 2) (/ (:height g) 2)  0)
        up     (p/cross (p/subtract focus camera) (p/subtract (p/point (:width g) 0 0) (p/point 0 (:height g) 0)))
        projector (projection/projector camera focus up width height)
        scale (/ width (:width g))
        pject (fn [v] (projection/project projector (p/point (* scale (nth v 0)) (* scale (nth v 1)) (* 5 (nth v 2)))))
        triangles (create-triangles g pject)
        triangles (map #(vector (nth % 0) (nth % 1)) triangles)
        ]
    (dorun (map #(let [[t1 t2] %]
                   (draw-polygon graphics t1 [128 20 128 255] [128 20 128 255])
                   (draw-polygon graphics t2 [128 40 128 255] [80 10 80 255]))
                triangles))))

(defn render-triangulation
  "Render the grid using a triangulation."
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        tmp (draw-triangles g config graphics width height)
        tmp (.dispose graphics)]
    (image/write-image filename img)))



(defn debug-render
  "Render the grid using a triangulation."
  [filename width height]
  (let [config default-config
        [img graphics] (new-image config width height)

        camera (p/point 1 15 10)
        focus  (p/point 0 0  0)
        up     (p/point 0 1  0)
        projector (projection/projector camera focus up width height)
        pject (fn [v] (projection/project projector v))

        p1 (p/point -90  0 -10)
        p2 (p/point  90  0 -10)
        p3 (p/point   0 90 -10)
        p4 (p/point   0  0   5)

        pt1 (pject p1)
        pt2 (pject p2)
        pt3 (pject p3)
        pt4 (pject p4)

        tmp (dorun (println (c/out p1)))
        tmp (dorun (println (c/out pt1)))
        tmp (dorun (println (c/out p2)))
        tmp (dorun (println (c/out pt2)))
        tmp (dorun (println (c/out p3)))
        tmp (dorun (println (c/out pt3)))
        tmp (dorun (println (c/out p4)))
        tmp (dorun (println (c/out pt4)))

        t1 (list pt1 pt2 pt3)
        t2 (list pt1 pt2 pt4)
        t3 (list pt1 pt3 pt4)
        t4 (list pt2 pt3 pt4)
        ;tmp (dorun (println (c/out (polygon/from-points ps))))

        ;tmp (dorun (println (c/out (polygon/from-points pps))))

        tmp (draw-polygon graphics (polygon/from-points t1) [255 30 30 255] [128 20 128 255])
        tmp (draw-polygon graphics (polygon/from-points t2) [30 255 30 255] [128 20 128 255])
        tmp (draw-polygon graphics (polygon/from-points t3) [30 30 255 255] [128 20 128 255])
        tmp (draw-polygon graphics (polygon/from-points t4) [30 30 30 255] [128 20 128 255])

        tmp (.dispose graphics)]
    (image/write-image filename img)))

