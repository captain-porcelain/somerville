;; My own projection
;; First set up the camera at a point and a projection plane.
;; Then intersect the line from the camera to the point that is to be projected onto the plane.
;; Lastly transform the point from the plane into a 2d view by calculating a linear combination of the vector from the center
;; to that point that is then retransformed to a vector with the corresponding vectors in the 2d plane.

(ns somerville.geometry.projection.projection
  (:require
    [somerville.geometry.line :as l]
    [somerville.geometry.point :as p]
    [somerville.geometry.plane :as pl]
    [somerville.geometry.commons :as c]))

(defrecord Projector [camera focus up projection-plane screen-center width height horizontal-base vertical-base]
  c/Printable
  (c/out [this i] (str (c/indent i) "Projector settings\n"
                       "Camera at " (c/out camera) "\n"
                       "Focussing on " (c/out focus) "\n"
                       "Plane up is " (c/out up) "\n"
                       "Projection plane is " (c/out projection-plane) "\n"
                       "Screen center is " (c/out screen-center) "\n"
                       "Screen dimensions are " width "x" height "\n"
                       "Horizontal base is " (c/out horizontal-base) "\n"
                       "Vertical base is " (c/out vertical-base)))
  (c/out [this] (c/out this 0)))

(defn projection-plane
  "Calculate the projection plane for the given camera and focus points considering the orientation given by up."
  [camera focus up distance]
  (let [v1 (p/scale (p/normalize (p/subtract focus camera)) distance)
        v2 (p/add (p/add camera v1) up)
        v3 (p/cross v1 v2)]
    (pl/plane focus v2 v3)))

(defn projector
  "Create a projector that looks from the camera point to the focus point and a screen of width x height.
  The orientation of the screen plane is determined by the up vector."
  [camera focus up distance width height]
  (Projector.
    camera focus up
    (projection-plane camera focus up distance)
    (p/point (/ width 2) (/ height 2))
    width height
    (p/normalize (p/cross (p/subtract focus camera) up))
    (p/normalize up)))

(defn project
  "Project the given point using the projector settings."
  [projector point]
  (let [i (pl/intersect (:projection-plane projector) (l/line (:camera projector) point))
        vi (p/subtract i (:focus projector))
        [a b] (p/linear-combination vi (:horizontal-base projector) (:vertical-base projector))
        p2d (p/add (p/add (:screen-center projector) (p/point a 0)) (p/point 0 b))]
    p2d))
