(ns somerville.geometry.triangle
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [taoensso.timbre :as log]))


(defrecord Triangle [p1 p2 p3]
  c/Printable
  (c/out [this i] (str (c/indent i) "Triangle of points\n"
                              (c/out p1 (inc i)) "\n" (c/out p2 (inc i)) "\n" (c/out p3 (inc i))))
  (c/out [this] (c/out this 0)))

(defn triangle
  [p1 p2 p3]
  (Triangle. p1 p2 p3))

