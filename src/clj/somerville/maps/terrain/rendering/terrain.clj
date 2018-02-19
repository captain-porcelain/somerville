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
    [quil.core :as quil]))

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
        project   (partial projection/project-1 size width height)]
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
        tmp (.setPaint graphics (get-color fill-color))
        tmp (.fillPolygon graphics p)
        tmp (.setPaint graphics (get-color line-color))
        tmp (.drawPolygon graphics p)]))

(defn grid-points
  [g]
  (let [size (:width g)]
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
  (let [size (:width g)]
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
  (let [size (:width g)
        ;pject (fn [v] (projection/project-1 size width height (nth v 0) (nth v 1) (nth v 2)))
        px (p/point 1 0 -1)
        py (p/point 0 1 0)
        projector (projection/make-projector px py)
        pject (fn [v] (projector (p/point (nth v 0) (nth v 1) (nth v 2))))
        triangles (create-triangles g pject)
        offset (- 0 (found-min triangles :x))
        scale (/ (scale-factor triangles width offset) 2)
        tmp (dorun (println (str "Found max: " (found-max triangles :x) ", scale: " scale ", offset: " offset)))
        offset 1
        ]
    (dorun (map #(let [[t1 t2] %]
                   (draw-polygon graphics t1 [30 30 30 255] [128 20 128 255])
                   (draw-polygon graphics t2 [30 30 30 255] [80 10 80 255]))
                triangles))))

(defn render-triangulation
  "Render the grid using a triangulation."
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        tmp (draw-triangles g config graphics width height)
        tmp (.dispose graphics)]
    (image/write-image filename img)))

(def view-triangles (atom nil))

(def w (atom nil))

(def ox (atom 0))
(def oy (atom 0))
(def oz (atom 0))
(def rx (atom 0))
(def ry (atom 0))
(def rz (atom 0))
(def alt (atom false))

(defn draw-t
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 0 255 0)
  (quil/fill-float 0 255 0)
  (quil/lights)
  (quil/translate @ox @oy @oz)
  (quil/rotate-x @rx)
  (quil/rotate-y @ry)
  (quil/rotate-z @rz)
  (when-not (nil? @view-triangles)
    (dorun
      (for [t @view-triangles]
        (let [[p1 p2 p3] (polygon/to-points t)
              ho (* 3/4 (quil/height))]
          (quil/begin-shape :triangles)
          (quil/vertex (:x p1) (+ ho (:y p1)) (* -1 (:y p1)))
          (quil/vertex (:x p2) (+ ho (:y p2)) (* -1 (:y p2)))
          (quil/vertex (:x p3) (+ ho (:z p3)) (* -1 (:y p3)))
          (quil/end-shape))))))

(defn draw-b
  "This function is called by quil repeatedly."
  []
  (quil/background-float 0)
  (quil/stroke-float 20 20 20)
  (quil/fill-float 128 20 128)
  (quil/lights)
  (quil/translate @ox @oy @oz)
  (quil/rotate-x @rx)
  (quil/rotate-y @ry)
  (quil/rotate-z @rz)
  (when-not (nil? @w)
    (dorun
      (for [x (range (:width @w))
            y (range (:width @w))]
        (let [z (grid/get-from @w x y)]
          (quil/translate (* 10 x) 0 (* 10 y))
          (quil/box 10 (* 10 z) 10)
          (quil/translate (* -10 x) 0 (* -10 y))))))
  )

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (quil/fill 226)
  (quil/frame-rate 1))

(defn key-released []
  (if (= (quil/key-code) 18) ; alt
    (reset! alt false)))

(defn key-pressed []
  ;(dorun (println (str "pressed code " (quil/key-code))))
  (if (= (quil/key-code) 18) ; alt
    (reset! alt true))
  (if (= (quil/key-code) 39) ; right
    (if @alt
      (swap! rx #(+ % 0.1))
      (swap! ox #(+ % 10))))
  (if (= (quil/key-code) 37) ; left
    (if @alt
      (swap! rx #(- % 0.1))
      (swap! ox #(- % 10))))
  (if (= (quil/key-code) 38) ; up
    (if @alt
      (swap! ry #(+ % 0.1))
      (swap! oy #(+ % 10))))
  (if (= (quil/key-code) 40) ; down
    (if @alt
      (swap! ry #(- % 0.1))
      (swap! oy #(- % 10))))
  (if (= (quil/key-code) 43) ; +
    (if @alt
      (swap! rz #(+ % 0.1))
      (swap! oz #(+ % 10))))
  (if (= (quil/key-code) 45) ; down
    (if @alt
      (swap! rz #(- % 0.1))
      (swap! oz #(- % 10)))))

(defn show []
  (quil/sketch
    :title "Gaia Triangulation"
    :renderer :p3d
    :setup setup
    :draw draw-t
    :key-released key-released
    :key-pressed key-pressed
    :size [200 200]))

