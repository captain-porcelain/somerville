;; See https://www.redblobgames.com/x/1842-delaunay-voronoi-sphere/
(ns somerville.visualization.world
  (:require
    [somerville.geometry.commons :as commons]
    [somerville.geometry.line :as line]
    [somerville.geometry.point :as point]
    [somerville.geometry.sphere :as sphere]
    [somerville.geometry.projection.stereographic :as proj]
    [somerville.geometry.delaunay :as delaunay]
    [somerville.color.color :as color]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def width 1200)
(def height 800)

(def colors
  {:background     (color/rgba  10  10  10)
   :point-voronoi  (color/rgba   0 204 102)
   :point-delaunay (color/rgba 247  92   3)
   :line-voronoi   (color/rgba 241 196  15)
   :line-delaunay  (color/rgba 217  60 110)})


;;====================================================================================================
;; Data Handling

(defn line-to-sphere
  "Map the points of a line onto a sphere."
  [l]
  (line/line (proj/to-sphere (:p1 (:line l))) (proj/to-sphere (:p2 (:line l)))))

(defn to-voronoi
  "Create voronoi for points on a sphere."
  [points]
  (map line-to-sphere (delaunay/voronoi (delaunay/delaunay (map proj/to-plane points)))))

(def points (reagent/atom (sphere/fibonacci 100)))
(def lines (reagent/atom (to-voronoi @points)))


;;====================================================================================================
;; Drawing Functionality

(defn draw-point
  [p]
  (quil/stroke 0 128 255)
  (quil/fill 0 128 255)
  (quil/stroke-weight 1)
  (let [sp (point/scale p 100)]
    (quil/with-translation [(:x sp) (:y sp) (:z sp)]
      (quil/sphere 1))))

(defn draw-line
  [l]
  (quil/stroke 255 128 0)
  (quil/fill 255 128 0)
  (quil/stroke-weight 1)
  (let [p1 (point/scale (:p1 l) 100)
        p2 (point/scale (:p2 l) 100)]
    (quil/line (:x p1) (:y p1) (:z p1) (:x p2) (:y p2) (:z p2))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/camera 300 300 0 0 0 0 0 0 -1)
  (quil/background 0)
  (quil/stroke 222 0 128)
  (quil/no-fill)
  (quil/rect 0 0 width height)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  (dorun
    (for [p @points]
      (draw-point p)))
  (dorun
    (for [l @lines]
      (draw-line l))))


;;====================================================================================================
;; App Setup

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (let [bg (:background colors)]
    (quil/fill (:r bg) (:g bg) (:b bg) (:a bg)))
  (quil/frame-rate 10))

(defn init
  "Initialize Quil sketch."
  [canvas-id]
  (quil/defsketch world-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :renderer :p3d))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Voronoi World Generation"]
   [:h3 "Usage"]
   [:span
    "No interactions at this stage"]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span
    [:ul
     [:li (str "Count points: " (count @points))]
     [:li (str "Count lines: " (count @lines))]]]])

(defn ui
  "Draw the basic ui for this visualization."
  [props]
  [:div {:class "row"}
   [:div {:id "hostelement" :class "column left" :on-load init}]
   [:div {:class "column right"}
    [usage]
    [settings]]])

(defn visualize
  "Render html and canvas for terrain visualization."
  [props]
  (reagent/create-class
    {:reagent-render ui
     :component-did-mount #(init "hostelement")}))

