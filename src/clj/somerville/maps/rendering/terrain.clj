(ns somerville.maps.rendering.terrain
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite RenderingHints Polygon BasicStroke]
    [java.awt.image BufferedImage])
  (:require
    [somerville.image :as image]
    [somerville.commons :as commons]
    [somerville.maps.grid :as grid]))

;;=======================================================================================================================
;; Rendering the world

(defn iso
  [size x y]
  {:x (* 0.5 (- (+ size x) y))
   :y (* 0.5 (+ x y))})

(defn project
  [size width height x y z]
  (let [point (iso size x y)
        x0    (* width 0.5)
        y0    (* height 0.2)
        x     (* 6 (- (:x point) (* size 0.5)))
        y     (* 1 (inc (* 0.005 (- size (:y point)))))
        z     (- (* size 3) (+ (* 0.4 z) (* (:y point) 0.05)))]
    {:x (+ x0 (/ x y))
     :y (+ y0 (/ z y))}))

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

(defn render
  [g config filename width height]
  (let [[img graphics] (new-image config width height)
        tmp (draw g config graphics width height)
        tmp (.dispose graphics)]
    (image/write-image filename img)))

