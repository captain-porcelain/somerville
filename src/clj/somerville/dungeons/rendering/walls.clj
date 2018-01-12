(ns somerville.dungeons.rendering.walls
  (:import
    [java.awt Color Graphics2D Rectangle AlphaComposite]
    [java.awt.image BufferedImage])
  (:require
    [somerville.image :as image]
    [somerville.commons :as commons]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.dungeons.generators.grid :as grid]))

;==================================================================================================================
; General printing of grids


(defn make-rect-walker
  "Create walker for grids based on rectangles."
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

(defn make-hex-walker
  "Create walker for grids based on hex fields."
  [wall-length]
  (fn
    [g c]
    (when (< 0 (count (:links c)))
      (let [hwl (int (Math/floor (/ wall-length 2)))
            h (int (Math/floor (Math/sqrt (- (* wall-length wall-length) (* hwl hwl)))))
            cx (+ wall-length (* 3 (:x c) hwl))
            cy (if (even? (:x c)) (* 2 h (inc (:y c))) (* h (inc (* (:y c) 2))))
            x1 (- cx wall-length)
            x2 (- cx hwl)
            x3 (+ cx hwl)
            x4 (+ wall-length cx)
            y1 (- cy h)
            y2 cy
            y3 (+ cy h)]
        (list
          (when (not (commons/in? :north      (:links c))) (l/line (p/point x2 y1 ) (p/point x3 y1)))
          (when (not (commons/in? :south      (:links c))) (l/line (p/point x2 y3 ) (p/point x3 y3)))
          (when (not (commons/in? :north-west (:links c))) (l/line (p/point x1 y2 ) (p/point x2 y1)))
          (when (not (commons/in? :south-west (:links c))) (l/line (p/point x1 y2 ) (p/point x2 y3)))
          (when (not (commons/in? :north-east (:links c))) (l/line (p/point x3 y1 ) (p/point x4 y2)))
          (when (not (commons/in? :south-east (:links c))) (l/line (p/point x3 y3 ) (p/point x4 y2))))))))

(defn convert-to-walls
  "Convert grid to list of lines representing relevant walls."
  [g wall-length]
  (filter
    #(not (nil? %))
    (reduce concat
            (grid/walk-result g
                              (case (:grid-type g)
                                :rect (make-rect-walker wall-length)
                                :hex  (make-hex-walker wall-length))))))

(defn walls
  "Create set of walls for a grid."
  [g wall-length]
  (into #{} (map #(str "line " (:x (:p1 %)) "," (:y (:p1 %)) " " (:x (:p2 %)) "," (:y (:p2 %))) (convert-to-walls g wall-length))))

(defn new-image
  "Create a new image to hold the finished tiles."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))

(defn spit-walls
  "SPit walls to file."
  [g wall-length filename]
  (spit filename (clojure.string/join "\n" (walls g wall-length))))

(defn render-walls
  "Render grid walls to image."
  [g wall-length imagename]
  (let [walls (convert-to-walls g wall-length)
        hwl (int (Math/floor (/ wall-length 2)))
        h (int (Math/floor (Math/sqrt (- (* wall-length wall-length) (* hwl hwl)))))
        iw (+ hwl (* 3 hwl (:width g)) 10)
        ih (+ h (* 2 h (:height g)) 10)
        img ^BufferedImage (new-image iw ih)
        graphics ^Graphics2D (.createGraphics img)
        tmp (.setPaint graphics Color/white)
        tmp (.fill graphics (Rectangle. 0 0 iw ih))
        tmp (.setPaint graphics Color/black)
        tmp (dorun (map #(.drawLine graphics (+ 4 (:x (:p1 %))) (+ 4 (:y (:p1 %))) (+ 4 (:x (:p2 %))) (+ 4 (:y (:p2 %)))) walls))
        tmp (.dispose graphics)]
    (image/write-image imagename img)))
