(ns somerville.geometry.rectangle
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]))

;; Define a rectangle by two points
(defrecord Rectangle [p1 p2 p3 p4]
  c/Printable
  (c/out [this i] (str (c/indent i) "Rectangle from " (c/out p1) " to " (c/out p2) " to " (c/out p3) " to " (c/out p4)))
  (c/out [this] (c/out this 0)))

(defn rectangle
  "Get a rectangle from two points."
  [p1 p2]
  (let [points (sort (list p1 (p/point (p/x p1) (p/y p2)) p2 (p/point (p/x p2) (p/y p1))))]
    (Rectangle. (nth points 0) (nth points 1) (nth points 2) (nth points 3))))

