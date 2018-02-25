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




(defn render-heights
  [g config filename width height])
