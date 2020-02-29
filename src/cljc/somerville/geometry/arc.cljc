(ns somerville.geometry.arc
  (:require
    [taoensso.timbre :as log]
    [somerville.geometry.commons :as c]
    [somerville.geometry.line :as l]
    [somerville.geometry.rectangle :as r]
    [somerville.geometry.point :as p]
    [somerville.geometry.circle :as circle]))

;; Define an arc as the part of a circle between two points counter clockwise.
(defrecord Arc2 [c p1 p2]
  c/Printable
  (c/out [this i] (str (c/indent i) "Arc for " (c/out c) " between " p1 " and " p2))
  (c/out [this] (c/out this 0)))

(defn arc
  "Create an arc."
  [c p1 p2]
  (Arc2. c p1 p2))

(defn from-lines
  "Create an arc by intersecting two line segments with a circle."
  [c l1 l2]
  (let [i1 (first (circle/intersect-line-segment c l1))
        i2 (first (circle/intersect-line-segment c l2))
        tmp (log/info (c/out i1))
        tmp (log/info (c/out i2))
        ]
   (arc c i1 i2)))
