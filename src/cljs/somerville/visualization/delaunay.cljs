(ns somerville.visualization.delaunay
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.line :as line]
    [somerville.geometry.point :as p]
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

(def points (reagent/atom (list)))
(def delaunay-triangles (atom (delaunay/delaunay @points (p/point width height))))
(def voronoi-lines (atom (delaunay/voronoi @delaunay-triangles)))
(def draw-delaunay (reagent/atom true))
(def draw-voronoi (reagent/atom true))

(defn add-point!
  "Add another point to the diagram."
  [point]
  (reset! points (cons point @points))
  (reset! delaunay-triangles (delaunay/add-point @delaunay-triangles point))
  (reset! voronoi-lines (delaunay/voronoi @delaunay-triangles)))

(defn clear!
  "Clear the diagram."
  []
  (reset! points (list))
  (reset! delaunay-triangles (delaunay/delaunay @points (p/point width height)))
  (reset! voronoi-lines (list)))

(defn toggle-drawing-delaunay!
  "Toggle drawing the delaunay triangles."
  []
  (reset! draw-delaunay (not @draw-delaunay)))

(defn toggle-drawing-voronoi!
  "Toggle drawing the voronoi diagram."
  []
  (reset! draw-voronoi (not @draw-voronoi)))

;;====================================================================================================
;; Drawing Functionality

(defn draw-point
  "Draw voronoi points."
  [p]
  (let [pv (:point-voronoi colors)]
    (do
      (quil/stroke (:r pv) (:g pv) (:b pv) (:a pv))
      (quil/fill (:r pv) (:g pv) (:b pv) (:a pv))
      (quil/rect (:x p) (:y p) 4 4))))

(defn draw-line
  "Draw voronoi line."
  [l]
  (let [lv (:line-voronoi colors)]
    (do
      (quil/stroke (:r lv) (:g lv) (:b lv) (:a lv))
      (quil/fill (:r lv) (:g lv) (:b lv) (:a lv))
      (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l))))))

(defn draw-triangle
  "Draw delaunay triangle"
  [t]
  (let [pd (:point-delaunay colors)]
    (do
      (quil/stroke (:r pd) (:g pd) (:b pd) (:a pd))
      (quil/fill (:r pd) (:g pd) (:b pd) (:a pd))
      (quil/rect (:x (:p (:c t))) (:y (:p (:c t))) 4 4)))
  (let [ld (:line-delaunay colors)]
    (do
      (quil/stroke (:r ld) (:g ld) (:b ld) (:a ld))
      (quil/fill (:r ld) (:g ld) (:b ld) (:a ld))
      (quil/line (:x (:p1 (:t t))) (:y (:p1 (:t t))) (:x (:p2 (:t t))) (:y (:p2 (:t t))))
      (quil/line (:x (:p2 (:t t))) (:y (:p2 (:t t))) (:x (:p3 (:t t))) (:y (:p3 (:t t))))
      (quil/line (:x (:p3 (:t t))) (:y (:p3 (:t t))) (:x (:p1 (:t t))) (:y (:p1 (:t t)))))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (let [bg (:background colors)]
    (quil/background (:r bg) (:g bg) (:b bg) (:a bg)))
  (dorun
    (for [p @points]
      (draw-point p)))
  (when @draw-delaunay
    (dorun
      (for [t (delaunay/remove-bounds @delaunay-triangles)]
        (draw-triangle t))))
  (when @draw-voronoi
    (dorun
      (for [l @voronoi-lines]
        (draw-line (:line l))))))


;;====================================================================================================
;; Event Handling

(defn mouse-released
  "Handle releasing mouse buttons."
  []
  (add-point! (p/point (quil/mouse-x) (quil/mouse-y))))

(defn key-pressed []
  "Trigger actions on key presses."
  (case (quil/key-as-keyword)
    :c (clear!)
    :d (toggle-drawing-delaunay!)
    :v (toggle-drawing-voronoi!)
    (dorun (println (str "pressed key " (quil/key-as-keyword))))))


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
  (quil/defsketch delaunay-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :mouse-released mouse-released
    :key-pressed key-pressed))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Voronoi by Delaunay"]
   [:h3 "Usage"]
   [:span
    "Press"
    [:ul
     [:li "left mouse button to add point"]
     [:li "c to clear diagram"]
     [:li "d to toggle drawing delaunay"]
     [:li "v to toggle drawing voronoi"]]]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span
    [:ul
     [:li (str "Count points: " (count @points))]
     [:li (str "Drawing delaunay: " @draw-delaunay)]
     [:li (str "Drawing voronoi: " @draw-voronoi)]]]])

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


