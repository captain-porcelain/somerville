(ns somerville.rasterization.grid-walls
  (:require
    [somerville.commons :as commons]
    [somerville.geometry.point :as point]
    [somerville.geometry.polygon :as poly]
    [somerville.geometry.line :as line]
    [somerville.maps.grid :as grid]))

;==================================================================================================================
; Walking through grids

(defn rect-coordinates
  "Get coordinates for a rectangular cell."
  [wall-length x y]
  (let [x1 (* wall-length x)
        y1 (* wall-length y)
        x2 (* wall-length (inc x))
        y2 (* wall-length (inc y))]
    [(point/point x2 y1) (point/point x2 y2) (point/point x1 y2)]))

(defn make-rect-wall-walker
  "Create walker for grids based on rectangles returning lines for walls."
  [wall-length]
  (fn
    [g c]
    (when (< 0 (count (:links c)))
      (let [[p1 p2 p3 p4] (rect-coordinates wall-length (:x c) (:y c))]
        (list
          (when (not (commons/in? :north (:links c))) (line/line p1 p2))
          (when (not (commons/in? :south (:links c))) (line/line p4 p3))
          (when (not (commons/in? :west  (:links c))) (line/line p1 p4))
          (when (not (commons/in? :east  (:links c))) (line/line p2 p3)))))))

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
    [(point/point x1 y2)
     (point/point x2 y1)
     (point/point x3 y1)
     (point/point x4 y2)
     (point/point x3 y3)
     (point/point x2 y3)]))

(defn make-hex-wall-walker
  "Create walker for grids based on hex fields returning lines for walls."
  [wall-length]
  (fn
    [g c]
    (when (< 0 (count (:links c)))
      (let [[p1 p2 p3 p4 p5 p6] (hex-coordinates wall-length (:x c) (:y c))]
        (list
          (when (not (commons/in? :north      (:links c))) (line/line p2 p3))
          (when (not (commons/in? :south      (:links c))) (line/line p6 p5))
          (when (not (commons/in? :north-west (:links c))) (line/line p1 p2))
          (when (not (commons/in? :south-west (:links c))) (line/line p1 p6))
          (when (not (commons/in? :north-east (:links c))) (line/line p3 p4))
          (when (not (commons/in? :south-east (:links c))) (line/line p5 p4)))))))

(defn make-hex-polygon-walker
  "Create walker for grids based on hex fields returning polygons."
  [wall-length]
  (fn
    [g c]
    (when (< 0 (count (:links c))) (poly/from-points (hex-coordinates wall-length (:x c) (:y c))))))


;==================================================================================================================
; Converting grids to renderable objects

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


;==================================================================================================================
; Converting grids to textual wall descriptions for use in discovery algorithms

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


;==================================================================================================================
; Special handling for entrances to grids

(defn hex-entrance-right
  "Create polygon to draw entrance on the right side for hex dungeon."
  [ps width height border]
  (let [p1 (nth ps 2)
        tp1 (point/point-at p1 (* -1 (/ Math/PI 6)) 300)
        l1 (line/line p1 tp1)
        wl (line/line (point/point width 0) (point/point width height))
        i1 (line/intersect l1 wl)
        p2 (nth ps 4)
        tp2 (point/point-at p2 (/ Math/PI 6) 300)
        l2 (line/line p2 tp2)
        i2 (line/intersect l2 wl)]
    (poly/from-points (list p1 i1 i2 p2))))

(defn hex-entrance-left
  "Create polygon to draw entrance on the left side for hex dungeon."
  [ps width height border]
  (let [p1 (nth ps 1)
        tp1 (point/point-at p1 (/ Math/PI 6) 300)
        l1 (line/line p1 tp1)
        wl (line/line (point/point 0 0) (point/point 0 height))
        i1 (line/intersect l1 wl)
        p2 (nth ps 5)
        tp2 (point/point-at p2 (* -1 (/ Math/PI 6)) 300)
        l2 (line/line p2 tp2)
        i2 (line/intersect l2 wl)
        mi1 (point/point (- (:x i1) border) (:y i1))
        mi2 (point/point (- (:x i2) border) (:y i2))]
    (poly/from-points (list p1 mi1 mi2 p2))))

(defn hex-entrance-top
  "Create polygon to draw entrance on the top for hex dungeon."
  [ps width height border]
  (let [p1 (nth ps 1)
        tp1 (point/point-at p1 (* -4 (/ Math/PI 6)) 300)
        l1 (line/line p1 tp1)
        wl (line/line (point/point 0 0) (point/point width 0))
        i1 (line/intersect l1 wl)
        p2 (nth ps 2)
        tp2 (point/point-at p2 (* -2 (/ Math/PI 6)) 300)
        l2 (line/line p2 tp2)
        i2 (line/intersect l2 wl)
        mi1 (point/point (:x i1) (- (:y i1) border))
        mi2 (point/point (:x i2) (- (:y i2) border))]
    (poly/from-points (list p1 mi1 mi2 p2))))

(defn hex-entrance-bottom
  "Create polygon to draw entrance on the bottom for hex dungeon."
  [ps width height border]
  (let [p1 (nth ps 4)
        tp1 (point/point-at p1 (* 2 (/ Math/PI 6)) 300)
        l1 (line/line p1 tp1)
        wl (line/line (point/point 0 height) (point/point width height))
        i1 (line/intersect l1 wl)
        p2 (nth ps 5)
        tp2 (point/point-at p2 (* 4 (/ Math/PI 6)) 300)
        l2 (line/line p2 tp2)
        i2 (line/intersect l2 wl)]
    (poly/from-points (list p1 i1 i2 p2))))

(defn entrance-polygon
  "Get polygon representing entrance."
  [g config width height]
  (case (:grid-type g)
    :rect (let [[p1 p2 p3 p4] (rect-coordinates (:wall-length config) (:x (:start g)) (:y (:start g)))
                border (:border config)]
            (cond
              (= (:y (:start g)) 0)                 (poly/from-points (list p1 (point/point (:x p1) (- 0 border)) (point/point (:x p2) (- 0 border)) p2))
              (= (:y (:start g)) (dec (:height g))) (poly/from-points (list p1 (point/point (:x p1) (+ height border)) (point/point (:x p2) (+ height border)) p2))
              (= (:x (:start g)) 0)                 (poly/from-points (list p1 (point/point (- 0 border) (:y p1)) (point/point (- 0 border) (:y p4)) p4))
              (= (:x (:start g)) (dec (:width g)))  (poly/from-points (list p1 (point/point (+ width border) (:y p1)) (point/point (+ width border) (:y p4)) p4))))
    :hex (let [ps (hex-coordinates (:wall-length config) (:x (:start g)) (:y (:start g)))]
           (cond
             (= (:x (:start g)) 0)                 (hex-entrance-left ps width height (:border config))
             (= (:x (:start g)) (- (:width g) 1))  (hex-entrance-right ps width height (:border config))
             (= (:y (:start g)) 0)                 (hex-entrance-top ps width height (:border config))
             (= (:y (:start g)) (- (:height g) 1)) (hex-entrance-bottom ps width height (:border config))))))

;; A default config for rasterizing grids
(def default-config
  {:wall-length           50
   :border                40})

