(ns somerville.visualization.terrain
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.line :as line]
    [somerville.geometry.point :as point]
    [somerville.color.color :as color]
    [somerville.maps.terrain.loki :as loki]
    [somerville.rasterization.conrec :as conrec]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def width 1200)
(def height 800)

(def colors
  {:background  (color/rgba  10  10  10)
   :line-low    (color/rgba 241 196  15)
   :line-high   (color/rgba 217  60 110)})


;;====================================================================================================
;; Data Handling

(def loki-detail (reagent/atom 3))
(def loki-config (atom (loki/default-config @loki-detail)))
(def world (atom nil))

(def contour-steps (reagent/atom 5))
(def contours (atom nil))
(def line-colors (reagent/atom (list (:line-low colors) (:line-high colors))))

(defn recreate-line-colors!
  "Recreate list of line colors fitting to contour lines."
  []
  (reset! line-colors (color/color-steps (:line-low colors) (:line-high colors) (count @contours))))

(defn recreate-world!
  "Recreate the world."
  []
  (log/info "Create terrain")
  (reset! loki-config (loki/default-config @loki-detail))
  (reset! world (loki/world @loki-config))
  (reset! contours (conrec/contour @world @contour-steps))
  (recreate-line-colors!)
  (log/info "Done"))

(defn recreate-contour!
  "Recreate the contouring."
  []
  (log/info "Create contour")
  (reset! contours (conrec/contour @world @contour-steps))
  (recreate-line-colors!)
  (log/info "Done"))

(defn increase-detail!
  "Increase the detail settings for loki."
  []
  (swap! loki-detail inc)
  (log/info "Loki details set to" @loki-detail))

(defn decrease-detail!
  "Decrease the detail settings for loki."
  []
  (when (> @loki-detail 1)
    (do
      (swap! loki-detail dec)
      (log/info "Loki details set to" @loki-detail))))

(defn increase-steps!
  "Increase the step settings for conrec."
  []
  (swap! contour-steps inc)
  (log/info "Conrec steps set to" @contour-steps))

(defn decrease-steps!
  "Decrease the detail settings for loki."
  []
  (when (> @contour-steps 1)
    (do
      (swap! contour-steps dec)
      (log/info "Conrec steps set to" @contour-steps))))


;;====================================================================================================
;; Drawing Functionality

(defn draw-line
  "Draw one line in the given color."
  [line width height line-color]
  (quil/stroke (:r line-color) (:g line-color) (:b line-color) (:a line-color))
  (quil/fill (:r line-color) (:g line-color) (:b line-color) (:a line-color))
  (quil/line
    (- width  (:x (:p1 line)))
    (- height (:y (:p1 line)))
    (- width  (:x (:p2 line)))
    (- height (:y (:p2 line)))))

(defn draw-height-lines
  "Draw all lines at scale."
  [lines scale width height line-color]
  (dorun (map #(draw-line (line/scale % scale) width height line-color) lines)))

(defn draw
  "This function is called by quil repeatedly."
  []
  (let [bg (:background colors)]
    (quil/background (:r bg) (:g bg) (:b bg) (:a bg)))
  (quil/stroke-weight 2)
  (let [scale (/ width (:width @world))]
    (dorun
      (map #(draw-height-lines (:lines %1) scale width height %2) @contours @line-colors))))


;;====================================================================================================
;; Event Handling

(defn key-pressed
  "Trigger actions on key presses."
  []
  (case (quil/key-code)
    171 (increase-detail!) ;; +
    173 (decrease-detail!) ;; -
    76  (decrease-steps!) ;; l
    77  (increase-steps!) ;; m
    67  (recreate-contour!) ;; c
    84  (recreate-world!) ;; s
    (log/info "Pressed unhandled key with code" (quil/key-code))))

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
  (quil/defsketch terrain-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :key-pressed key-pressed))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Terrain Visualization"]
   [:h3 "Usage"]
   [:span
    "Press"
    [:ul
     [:li "t to create a new terrain"]
     [:li "c to recreate contours"]
     [:li "+ to increase terrain detail level"]
     [:li "- to decrease terrain detail level"]
     [:li "m to increase contour steps"]
     [:li "p to decrease contour steps"]]]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span
    [:ul
     [:li (str "Loki detail: " @loki-detail)]
     [:li (str "Contour steps: " @contour-steps)]
     [:li (str "Contour lines: " (count @contours))]
     [:li (str "Line colors: " (count @line-colors))]]]])

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


