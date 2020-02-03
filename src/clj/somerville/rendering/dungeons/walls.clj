(ns somerville.rendering.dungeons.walls
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite RenderingHints Polygon BasicStroke]
    [java.awt.image BufferedImage])
  (:require
    [somerville.rendering.image :as image]
    [somerville.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.polygon :as poly]
    [somerville.geometry.line :as l]
    [somerville.maps.grid :as grid]))

;==================================================================================================================
; General printing of grids

(defn rect-coordinates
  "Get coordinates for hex cell."
  [wall-length x y]
  (let [x1 (* wall-length x)
        y1 (* wall-length y)
        x2 (* wall-length (inc x))
        y2 (* wall-length (inc y))]
    [(p/point x1 y1) (p/point x2 y1) (p/point x2 y2) (p/point x1 y2)]))

(defn make-rect-wall-walker
  "Create walker for grids based on rectangles returning lines for walls."
  [wall-length]
  (fn
    [g c]
    (when (< 0 (count (:links c)))
      (let [[p1 p2 p3 p4] (rect-coordinates wall-length (:x c) (:y c))]
        (list
          (when (not (commons/in? :north (:links c))) (l/line p1 p2))
          (when (not (commons/in? :south (:links c))) (l/line p4 p3))
          (when (not (commons/in? :west  (:links c))) (l/line p1 p4))
          (when (not (commons/in? :east  (:links c))) (l/line p2 p3)))))))

(defn make-rect-polygon-walker
  "Create walker for grids based on rectangles returning polygons."
  [wall-length] (fn
    [g c]
    (when (< 0 (count (:links c)))
      (poly/from-points (rect-coordinates wall-length (:x c) (:y c))))))

(defn hex-coordinates
  "Get coordinates for hex cell."
  [wall-length x y]
  (let [hwl (int (Math/floor (/ wall-length 2)))
        h (int (Math/floor (Math/sqrt (- (* wall-length wall-length) (* hwl hwl)))))
        cx (+ wall-length (* 3 x hwl))
        cy (if (even? x) (* 2 h (inc y)) (* h (inc (* y 2))))
        x1 (- cx wall-length)
        x2 (- cx hwl)
        x3 (+ cx hwl)
        x4 (+ wall-length cx)
        y1 (- cy h)
        y2 cy
        y3 (+ cy h)]
    [(p/point x1 y2)
     (p/point x2 y1)
     (p/point x3 y1)
     (p/point x4 y2)
     (p/point x3 y3)
     (p/point x2 y3)]))

(defn make-hex-wall-walker
  "Create walker for grids based on hex fields returning lines for walls."
  [wall-length]
  (fn
    [g c]
    (when (< 0 (count (:links c)))
      (let [[p1 p2 p3 p4 p5 p6] (hex-coordinates wall-length (:x c) (:y c))]
        (list
          (when (not (commons/in? :north      (:links c))) (l/line p2 p3))
          (when (not (commons/in? :south      (:links c))) (l/line p6 p5))
          (when (not (commons/in? :north-west (:links c))) (l/line p1 p2))
          (when (not (commons/in? :south-west (:links c))) (l/line p1 p6))
          (when (not (commons/in? :north-east (:links c))) (l/line p3 p4))
          (when (not (commons/in? :south-east (:links c))) (l/line p5 p4)))))))

(defn make-hex-polygon-walker
  "Create walker for grids based on hex fields returning polygons."
  [wall-length]
  (fn
    [g c]
    (when (< 0 (count (:links c))) (poly/from-points (hex-coordinates wall-length (:x c) (:y c))))))

(defn convert-to-walls
  "Convert grid to list of lines representing relevant walls."
  [g wall-length]
  (filter
    #(not (nil? %))
    (reduce concat
            (grid/walk-result g
                              (case (:grid-type g)
                                :rect (make-rect-wall-walker wall-length)
                                :hex  (make-hex-wall-walker wall-length))))))

(defn convert-to-polygons
  "Convert grid to list of polygons representing relevant cells."
  [g wall-length]
  (filter
    #(not (nil? %))
    (grid/walk-result g
                      (case (:grid-type g)
                        :rect (make-rect-polygon-walker wall-length)
                        :hex  (make-hex-polygon-walker wall-length)))))

(defn wall-description
  "Create wall description and include border offset."
  ([l border]
   (str "line " (+ border (:x (:p1 l))) "," (+ border (:y (:p1 l))) " " (+ border (:x (:p2 l))) "," (+ border (:y (:p2 l)))))
  ([l]
   (wall-description l 0)))

(defn walls
  "Create set of walls for a grid."
  [g config]
  (let [wall-length (:wall-length config)]
    (into #{} (map #(wall-description % (:border config)) (convert-to-walls g wall-length)))))

(defn spit-walls
  "Spit walls to file."
  [g config filename]
  (spit filename (clojure.string/join "\n" (walls g config))))

(def default-config
  {:background-color      [0 0 0 255]
   :wall-color            [0 0 0 255]
   :transparent-cells     false
   :cell-background-color [255 255 255 255]
   :wall-width            3
   :wall-length           50
   :border                40})

(defn get-color
  "Convert integer array into Color."
  [c]
  (Color. ^Integer (nth c 0) ^Integer (nth c 1) ^Integer (nth c 2) ^Integer (nth c 3)))

(defn new-image
  "Create a new image to hold the finished tiles."
  [g config]
  (let [hwl (int (Math/floor (/ (:wall-length config) 2)))
        h (int (Math/floor (Math/sqrt (- (* (:wall-length config) (:wall-length config)) (* hwl hwl)))))
        hiw (+ hwl (* 3 hwl (:width g)) (* 2 (:border config)))
        hih (+ h (* 2 h (:height g)) (* 2 (:border config)))
        riw (+ (* (:wall-length config) (:width g)) (* 2 (:border config)))
        rih (+ (* (:wall-length config) (:height g)) (* 2 (:border config)))
        width (if (= :hex (:grid-type g)) hiw riw)
        height (if (= :hex (:grid-type g)) hih rih)
        img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        tmp (.setPaint graphics (get-color (:background-color config)))
        tmp (.fill graphics (Rectangle. 0 0 width height))]
    [img graphics]))

(defn draw-polygon
  "Draw a polygon onto the canvas."
  [graphics polygon border]
  (let [xs (into-array Integer/TYPE (map #(+ (:x %) border) (poly/to-points polygon)))
        ys (into-array Integer/TYPE (map #(+ (:y %) border) (poly/to-points polygon)))
        p (Polygon. xs ys (count xs))
        tmp (.fillPolygon graphics p)
        tmp (.drawPolygon graphics p)]))

(defn draw-line
  "Draw a line onto the canvas."
  [graphics line border]
  (.drawLine graphics (+ border (:x (:p1 line))) (+ border (:y (:p1 line))) (+ border (:x (:p2 line))) (+ border (:y (:p2 line)))))

(defn floor-tiles
  "Render floor tiles and overlay given wall drawing."
  [img config]
  (if-not (nil? (:floor-tile config))
    (let [image (BufferedImage. (.getWidth img) (.getHeight img) BufferedImage/TYPE_INT_ARGB)
          graphics ^Graphics2D (.createGraphics image)
          tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
          tmp (.setPaint graphics (get-color (:background-color config)))
          tile (image/load-image (:floor-tile config))
          tw (.getWidth tile)
          th (.getHeight tile)
          tmp (dorun (for [x (range (int (Math/ceil (/ (.getWidth img) tw))))
                           y (range (int (Math/ceil (/ (.getHeight img) th))))]
                       (.drawImage graphics tile (* x tw) (* y th) nil)))
          tmp (.drawImage graphics img 0 0 nil)
          tmp (.dispose graphics)]
      image)
    img))

(defn handle-cells
  "Handle drawing cells to image."
  [g graphics config]
  (if (:transparent-cells config)
    (do
      (.setComposite graphics (AlphaComposite/getInstance AlphaComposite/CLEAR))
      (.setStroke graphics (BasicStroke. 2))
      (dorun (map #(draw-polygon graphics % (:border config)) (convert-to-polygons g (:wall-length config))))
      (.setStroke graphics (BasicStroke. 1))
      (.setComposite graphics (AlphaComposite/getInstance AlphaComposite/SRC_OVER)))
    (do
      (.setStroke graphics (BasicStroke. 2))
      (.setPaint graphics (get-color (:cell-background-color config)))
      (dorun (map #(draw-polygon graphics % (:border config)) (convert-to-polygons g (:wall-length config))))
      (.setStroke graphics (BasicStroke. 1)))))

(defn handle-walls
  "Handle drawing walls to image."
  [g graphics config]
  (.setPaint graphics (get-color (:wall-color config)))
  (.setStroke graphics (BasicStroke. (:wall-width config)))
  (dorun (map #(draw-line graphics % (:border config)) (convert-to-walls g (:wall-length config))))
  (.setStroke graphics (BasicStroke. 1)))

(defn hex-entrance-right
  "Create polygon to draw entrance on the right side for hex dungeon."
  [ps width height border]
  (let [p1 (nth ps 2)
        tp1 (p/point-at p1 (* -1 (/ Math/PI 6)) 300)
        l1 (l/line p1 tp1)
        wl (l/line (p/point width 0) (p/point width height))
        i1 (l/intersect l1 wl)
        p2 (nth ps 4)
        tp2 (p/point-at p2 (/ Math/PI 6) 300)
        l2 (l/line p2 tp2)
        i2 (l/intersect l2 wl)]
    (poly/from-points (list p1 i1 i2 p2))))

(defn hex-entrance-left
  "Create polygon to draw entrance on the left side for hex dungeon."
  [ps width height border]
  (let [p1 (nth ps 1)
        tp1 (p/point-at p1 (/ Math/PI 6) 300)
        l1 (l/line p1 tp1)
        wl (l/line (p/point 0 0) (p/point 0 height))
        i1 (l/intersect l1 wl)
        p2 (nth ps 5)
        tp2 (p/point-at p2 (* -1 (/ Math/PI 6)) 300)
        l2 (l/line p2 tp2)
        i2 (l/intersect l2 wl)
        mi1 (p/point (- (:x i1) border) (:y i1))
        mi2 (p/point (- (:x i2) border) (:y i2))]
    (poly/from-points (list p1 mi1 mi2 p2))))

(defn hex-entrance-top
  "Create polygon to draw entrance on the top for hex dungeon."
  [ps width height border]
  (let [p1 (nth ps 1)
        tp1 (p/point-at p1 (* -4 (/ Math/PI 6)) 300)
        l1 (l/line p1 tp1)
        wl (l/line (p/point 0 0) (p/point width 0))
        i1 (l/intersect l1 wl)
        p2 (nth ps 2)
        tp2 (p/point-at p2 (* -2 (/ Math/PI 6)) 300)
        l2 (l/line p2 tp2)
        i2 (l/intersect l2 wl)
        mi1 (p/point (:x i1) (- (:y i1) border))
        mi2 (p/point (:x i2) (- (:y i2) border))]
    (poly/from-points (list p1 mi1 mi2 p2))))

(defn hex-entrance-bottom
  "Create polygon to draw entrance on the bottom for hex dungeon."
  [ps width height border]
  (let [p1 (nth ps 4)
        tp1 (p/point-at p1 (* 2 (/ Math/PI 6)) 300)
        l1 (l/line p1 tp1)
        wl (l/line (p/point 0 height) (p/point width height))
        i1 (l/intersect l1 wl)
        p2 (nth ps 5)
        tp2 (p/point-at p2 (* 4 (/ Math/PI 6)) 300)
        l2 (l/line p2 tp2)
        i2 (l/intersect l2 wl)]
    (poly/from-points (list p1 i1 i2 p2))))

(defn entrance-polygon
  "Get polygon representing entrance."
  [g config width height]
  (case (:grid-type g)
    :rect (let [[p1 p2 p3 p4] (rect-coordinates (:wall-length config) (:x (:start g)) (:y (:start g)))
                border (:border config)]
            (cond
              (= (:y (:start g)) 0)                 (poly/from-points (list p1 (p/point (:x p1) (- 0 border)) (p/point (:x p2) (- 0 border)) p2))
              (= (:y (:start g)) (dec (:height g))) (poly/from-points (list p1 (p/point (:x p1) (+ height border)) (p/point (:x p2) (+ height border)) p2))
              (= (:x (:start g)) 0)                 (poly/from-points (list p1 (p/point (- 0 border) (:y p1)) (p/point (- 0 border) (:y p4)) p4))
              (= (:x (:start g)) (dec (:width g)))  (poly/from-points (list p1 (p/point (+ width border) (:y p1)) (p/point (+ width border) (:y p4)) p4))))
    :hex (let [ps (hex-coordinates (:wall-length config) (:x (:start g)) (:y (:start g)))]
           (cond
             (= (:x (:start g)) 0)                 (hex-entrance-left ps width height (:border config))
             (= (:x (:start g)) (- (:width g) 1))  (hex-entrance-right ps width height (:border config))
             (= (:y (:start g)) 0)                 (hex-entrance-top ps width height (:border config))
             (= (:y (:start g)) (- (:height g) 1)) (hex-entrance-bottom ps width height (:border config))))))

(defn draw-entrance
  "Ensure entrance is open."
  [g graphics config width height]
  (.setComposite graphics (AlphaComposite/getInstance AlphaComposite/CLEAR))
  (.setStroke graphics (BasicStroke. 2))
  (draw-polygon graphics (entrance-polygon g config width height)  (:border config))
  (.setStroke graphics (BasicStroke. 1))
  (.setComposite graphics (AlphaComposite/getInstance AlphaComposite/SRC_OVER)))

(defn draw-image
  "Render grid walls to image."
  [g config]
  (let [[img graphics] (new-image g config)
        tmp (handle-cells g graphics config)
        tmp (handle-walls g graphics config)
        tmp (draw-entrance g graphics config (.getWidth img) (.getHeight img))
        tmp (.dispose graphics)]
    (floor-tiles img config)))

(defn save-image
  "Render grid walls to image."
  [g config imagename]
  (image/write-image imagename (draw-image g config)))

(defn image-and-walls
  "Get an image for the given grid and also the wall descriptions."
  [g config]
  (let [img (draw-image g config)
        walls (walls g config)
        entrance (entrance-polygon g config (.getWidth img) (.getHeight img))]
    {:image img
     :wall-description (clojure.string/join "\n"
                                            (clojure.set/union walls #{(wall-description (nth (:lines entrance) 0))
                                                                       (wall-description (nth (:lines entrance) 2))}))}))
