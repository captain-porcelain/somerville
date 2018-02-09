(ns somerville.dungeons.rendering.walls
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite RenderingHints Polygon BasicStroke]
    [java.awt.image BufferedImage])
  (:require
    [somerville.image :as image]
    [somerville.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.polygon :as poly]
    [somerville.geometry.line :as l]
    [somerville.dungeons.generators.grid :as grid]))

;==================================================================================================================
; General printing of grids


(defn make-rect-wall-walker
  "Create walker for grids based on rectangles returning lines for walls."
  [wall-length]
  (fn
    [g c]
    (when (< 0 (count (:links c)))
      (let [x1 (* wall-length (:x c))
            y1 (* wall-length (:y c))
            x2 (* wall-length (inc (:x c)))
            y2 (* wall-length (inc (:y c)))]
        (list
          (when (not (commons/in? :north (:links c))) (l/line (p/point x1 y1) (p/point x2 y1)))
          (when (not (commons/in? :south (:links c))) (l/line (p/point x1 y2) (p/point x2 y2)))
          (when (not (commons/in? :west  (:links c))) (l/line (p/point x1 y1) (p/point x1 y2)))
          (when (not (commons/in? :east  (:links c))) (l/line (p/point x2 y1) (p/point x2 y2))))))))

(defn make-rect-polygon-walker
  "Create walker for grids based on rectangles returning polygons."
  [wall-length] (fn
    [g c]
    (when (< 0 (count (:links c)))
      (let [x1 (* wall-length (:x c))
            y1 (* wall-length (:y c))
            x2 (* wall-length (inc (:x c)))
            y2 (* wall-length (inc (:y c)))]
        (poly/from-points (p/point x1 y1) (p/point x2 y1) (p/point x2 y2) (p/point x1 y2))))))

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

(defn walls
  "Create set of walls for a grid."
  [g wall-length]
  (into #{} (map #(str "line " (:x (:p1 %)) "," (:y (:p1 %)) " " (:x (:p2 %)) "," (:y (:p2 %))) (convert-to-walls g wall-length))))

(defn spit-walls
  "SPit walls to file."
  [g wall-length filename]
  (spit filename (clojure.string/join "\n" (walls g wall-length))))

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
        iw (+ hwl (* 3 hwl (:width g)) (* 2 (:border config)))
        ih (+ h (* 2 h (:height g)) (* 2 (:border config)))
        img (BufferedImage. iw ih BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setRenderingHint graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        tmp (.setPaint graphics (get-color (:background-color config)))
        tmp (.fill graphics (Rectangle. 0 0 iw ih))]
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

(defn render-walls
  "Render grid walls to image."
  [g config imagename]
  (let [[img graphics] (new-image g config)
        tmp (handle-cells g graphics config)
        tmp (handle-walls g graphics config)
        tmp (.dispose graphics)]
    (image/write-image imagename (floor-tiles img config))))

