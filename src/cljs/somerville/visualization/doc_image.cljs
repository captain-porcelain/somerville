(ns somerville.visualization.doc-image
  (:require
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as line]
    [somerville.color.color :as color]
    [quil.core :as quil :include-macros true]
    [taoensso.timbre :as log]
    [reagent.core :as reagent]))

(def width 1200)
(def height 800)

(def offset-width (atom (/ width 2)))
(def offset-height (atom (/ height 2)))

(def colors
  {:background      (color/rgba  40  40  40)
   :ui              (color/rgba 200 200 200)

   :point           (color/rgba 158   0  83)
   :line            (color/rgba 230 127  13)

   :point-delaunay  (color/rgba 254  78   0)
   :line-delaunay   (color/rgba 230 127  13)

   :point-invalid   (color/rgba  18  53  91)
   :line-invalid    (color/rgba  27 153 139)

   :line-highlight  (color/rgba 255 255 255)
   :point-highlight (color/rgba 255 255 255)

   :point-next      (color/rgba  57   0 153)})


(def definitions
  (atom
    [
     {:name "l1" :type :line  :object (line/line (p/point 100 100) (p/point 250 200)) :text-offset [ 10  10]}
     {:name "l2" :type :line  :object (line/line (p/point 100 100) (p/point 200 300)) :text-offset [ 10  10]}
     {:name "p0" :type :point :object (p/point 100 100) :text-offset [-10 -10]}
     {:name "p1" :type :point :object (p/point 250 200) :text-offset [ 10 -10]}
     {:name "p2" :type :point :object (p/point 200 300) :text-offset [ 10  10]}
     ]))

;;====================================================================================================
;; Drawing Functionality

(defn draw-axis
  "Draw the axis"
  []
  (let [col (:ui colors)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill (:r col) (:g col) (:b col) (:a col))
      (quil/line 0 @offset-height width @offset-height)
      (quil/line @offset-width 0 @offset-width height))))

(defn draw-point
  "Draw a point."
  [object]
  (let [point (:object object)
        col (:point colors)
        tcol (:ui colors)
        x (+ (:x point) @offset-width)
        y (+ (:y point) @offset-height)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill (:r col) (:g col) (:b col) (:a col))
      (quil/stroke-weight 6)
      (quil/point x y)
      (quil/stroke-weight 1)
      (quil/stroke (:r tcol) (:g tcol) (:b tcol) (:a tcol))
      (quil/fill (:r tcol) (:g tcol) (:b tcol) (:a tcol))
      (quil/text (:name object) (+ x (first (:text-offset object))) (+ y (second (:text-offset object)))))))

(defn draw-line
  "Draw a line."
  [object]
  (let [line (:object object)
        col (:line colors)]
    (do
      (quil/stroke (:r col) (:g col) (:b col) (:a col))
      (quil/fill (:r col) (:g col) (:b col) (:a col))
      (quil/stroke-weight 3)
      (quil/line (+ (:x (:p1 line)) @offset-width) (+ (:y (:p1 line)) @offset-height) (+ (:x (:p2 line)) @offset-width) (+ (:y (:p2 line)) @offset-height))
      (quil/stroke-weight 1))))


(defn draw-object
  [o]
  (case (:type o)
    :point (draw-point o)
    :line  (draw-line o)
    nil))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background 0)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  (do (draw-axis))
  (dorun (map draw-object @definitions)))



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
  (quil/defsketch lsystem-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Documentation Rendering"]
   [:h3 "Usage"]
   [:span
    "Nothing to do..."
    ]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span "None..."
    ]])

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


