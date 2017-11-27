(ns somerville.dungeons.rendering.walls
  (:require
    [somerville.commons :as commons]
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
          (when (not (commons/in? :north (:links c))) (str "line " x1 "," y1 " " x2 "," y1))
          (when (not (commons/in? :south (:links c))) (str "line " x1 "," y2 " " x2 "," y2))
          (when (not (commons/in? :west  (:links c))) (str "line " x1 "," y1 " " x1 "," y2))
          (when (not (commons/in? :east  (:links c))) (str "line " x2 "," y1 " " x2 "," y2)))))))

(defn make-hex-walker
  "Create walker for grids based on hex fields."
  [wall-length]
  (fn
    [g c]
    (when (< 0 (count (:links c)))
      (let [hwl (int (Math/floor (/ wall-length 2)))
            h (int (Math/floor (Math/sqrt (- (* wall-length wall-length) (* hwl hwl)))))
            cx (+ wall-length (* 3 (:x c) hwl))
            x1 (- cx wall-length)
            x2 (- cx hwl)
            x3 (+ cx hwl)
            x4 (+ wall-length cx)
            cy (if (even? (:x c)) (* h (inc (:y c))) (* h (inc (* (:y c) 2))))
            y1 (- cy h)
            y2 cy
            y3 (+ cy h)]
        (list
          (when (not (commons/in? :north (:links c))) (str "line " x2 "," y1 " " x3 "," y1))
          (when (not (commons/in? :south (:links c))) (str "line " x2 "," y3 " " x3 "," y3))
          (when (not (commons/in? :north-west  (:links c))) (str "line " x1 "," y2 " " x2 "," y1))
          (when (not (commons/in? :south-west  (:links c))) (str "line " x1 "," y2 " " x2 "," y3))
          (when (not (commons/in? :north-east  (:links c))) (str "line " x3 "," y1 " " x4 "," y2))
          (when (not (commons/in? :south-east  (:links c))) (str "line " x3 "," y3 " " x4 "," y2)))))))

(defn walls
  "Create set of walls for a grid."
  [g wall-length]
  (into
    #{}
    (filter
      #(not (nil? %))
      (reduce concat
        (grid/walk-result g
                   (case (:grid-type g)
                     :rect (make-rect-walker wall-length)
                     :hex  (make-hex-walker wall-length)))))))
