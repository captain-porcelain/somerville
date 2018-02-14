(ns somerville.maps.rendering.terrain
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite RenderingHints Polygon BasicStroke]
    [java.awt.image BufferedImage])
  (:require
    [somerville.geometry.polygon :as polygon]
    [somerville.geometry.point :as p]
    [somerville.image :as image]
    [somerville.commons :as commons]
    [somerville.geometry.commons :as c]
    [somerville.maps.grid :as grid]))


;;=======================================================================================================================
;; Calculating projections
;; http://www.codeincodeblock.com/2012/03/projecting-3d-world-co-ordinates-into.html

(defrecord Camera [from to up angleh anglev zoom front back projection])

(defn camera
  ([from to up]
   (Camera. from to up 45.0 45.0 1.0 1.0 200.0 0))
  ([]
   (camera (p/point 0 -50 0) (p/point 0 50 0) (p/point 0 0 1))))

(defrecord Screen [center width height])

(defn screen
  [c w h]
  (Screen. c w h))

(defn basis-vectors
  [camera]
  (let [basis-b (p/normalize (p/subtract (:to camera) (:from camera)))
        basis-a (p/normalize (p/cross (:up camera) basis-b))
        basis-c (p/cross basis-b basis-a)]
    {:basis-a basis-a :basis-b basis-b :basis-c basis-c}))

(defn transpose-world-to-eye
  [w camera basis]
  (let [w2 (p/subtract w (:from camera))
        ex (+ (* (:x w2) (:x (:basis-a basis))) (* (:y w2) (:y (:basis-a basis))) (* (:z w2) (:z (:basis-a basis))))
        ey (+ (* (:x w2) (:x (:basis-b basis))) (* (:y w2) (:y (:basis-b basis))) (* (:z w2) (:z (:basis-b basis))))
        ez (+ (* (:x w2) (:x (:basis-c basis))) (* (:y w2) (:y (:basis-c basis))) (* (:z w2) (:z (:basis-c basis))))]
    (p/point ex ey ez)))

(defn transpose-eye-to-norm
  [e camera apertures]
  (if (= 0 (:projection camera))
    (let [d (/ (:zoom camera) (:y e))
          nx (* d (/ (:x e) (:h apertures)))
          ny (:y e)
          nz (* d (/ (:z e) (:v apertures)))]
      (p/normalize (p/point nx ny nz)))
    (let [nx (* (:zoom camera) (/ (:x e) (:h apertures)))
          ny (:y e)
          nz (* (:zoom camera) (/ (:z e) (:v apertures)))]
      (p/normalize (p/point nx ny nz)))))

(defn transpose-norm-to-screen
  [n screen]
  (p/point
    (int (- (:x (:center screen)) (/ (* (:width screen) (:x n)) 2)))
    (int (- (:y (:center screen)) (/ (* (:height screen) (:z n)) 2)))))

(defn transpose-world-to-screen
  [w camera screen apertures basis]
  (transpose-norm-to-screen (transpose-eye-to-norm (transpose-world-to-eye w camera basis) camera apertures) screen))

(defn to-rad
  [a]
  (* 2 Math/PI (/ a 360)))

(defn aperture
  [camera]
  {:h (Math/tan (to-rad (* (:angleh camera) (/ 0.01745329252 2))))
   :v (Math/tan (to-rad (* (:anglev camera) (/ 0.01745329252 2))))})

(defn iso-projection
  [size x y]
  (p/point (* 0.5 (- (+ size x) y)) (* 0.5 (+ x y))))

(defn project-wtf
  [size width height x y z]
  (let [cx (int (/ size 2))
        cy (int (/ size 2))
        cz size
        z (if (= 0 z) 1 z)
        f (- z cz)
        px (+ cx (* (- x cx) (/ f z)))
        py (+ cy (* (- y cy) (/ f z)))]
    (p/point px py)))

(defn project
  [size width height x y z]
  (let [camera (camera (p/point 0 0 200) (p/point (/ size 2) (/ size 2) 0) (p/point 0 0 1))
        screen (Screen. (p/point (/ width 2) (/ height 2)) width height)
        apertures (aperture camera)
        basis (basis-vectors camera)
        p3d (p/point x y z)]
    (transpose-world-to-screen p3d camera screen apertures basis)))

(defn project-old
  [size width height x y z]
  (let [point (iso-projection size x y)
        x0    (* width 0.5)
        y0    (* height 0.2)
        x     (* 6 (- (:x point) (* size 0.5)))
        y     (inc (* 0.005 (- size (:y point))))
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
  [graphics polygon line-color fill-color]
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

