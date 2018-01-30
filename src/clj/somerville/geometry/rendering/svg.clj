(ns somerville.geometry.rendering.svg
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as l]
    [somerville.geometry.polygon :as poly]
    [dali.io :as dali]))

(def style {:stroke :black :stroke-width 4 :fill :none})

;;-----------------------------------------------------------------------------
;; Line Rendering

(defn line-to-svg
  "Convert line to svg."
  [line]
  [:line
   style
   [(int (:x (:p1 line))) (int (:y (:p1 line)))]
   [(int (:x (:p2 line))) (int (:y (:p2 line)))]])

;;-----------------------------------------------------------------------------
;; Voronoi Rendering

(defn bounding-box
  "Create polygon for bounding box."
  [bx1 by1 bx2 by2]
  (let [p1 (p/point bx1 by1)
        p2 (p/point bx2 by1)
        p3 (p/point bx2 by2)
        p4 (p/point bx1 by2)
        l1 (l/line p1 p2)
        l2 (l/line p2 p3)
        l3 (l/line p3 p4)
        l4 (l/line p4 p1)
        cl1 (l/line p1 p3)
        cl2 (l/line p2 p4)
        i (l/intersect cl1 cl2)]
    (poly/polygon (list l1 l2 l3 l4) i)))

(defn restrict-to-bounding-box
  "Ensure all lines are inside the bounding box."
  [box lines]
  (filter #(not (nil? %)) (map #(poly/shorten-line box %) lines)))

(defn voronoi-to-lines
  "Collect all lines in the voronoi inside the bounding box and include box itself."
  [v]
  (let [box (bounding-box (:bx1 v) (:by1 v) (:bx2 v) (:by2 v))]
    (distinct
      (concat
        (:lines box)
        (restrict-to-bounding-box box (reduce concat (map :lines (:cells v))))))))

(defn voronoi-to-page
  "Convert voronoi to rendering with list of polygons."
  [v]
  (into []
    (concat
      [:dali/page]
      (map line-to-svg (voronoi-to-lines v)))))

(defn voronoi
  "Convert voronoi to svg."
  [v filename]
  (dali/render-svg (voronoi-to-page v) filename))

