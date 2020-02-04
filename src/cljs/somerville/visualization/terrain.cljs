(ns somerville.visualization.terrain
  (:require
    [somerville.geometry.commons :as c]
    [somerville.geometry.line :as line]
    [somerville.geometry.point :as point]
    [somerville.maps.terrain.loki :as loki]
    [somerville.rasterization.conrec :as conrec]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def width 1200)
(def height 800)

(def colors
  {:background   [32 32 32 255]
   :line         [128 20 128 255]
   :colors       [[0 161 228 255] [204 88 3 255] [137 252 0 255] [160 160 160 255] [240 240 240 255]]})

(def heights
  {:heights      [-1000 10 20 28 32]
   :height-steps 4})

;;====================================================================================================
;; Data Handling

(def loki-detail (atom 3))
(def loki-config (atom (loki/default-config @loki-detail)))
(def world (atom nil))

(def contour-steps (atom 5))
(def contours (atom nil))

(defn recreate-world!
  "Recreate the world."
  []
  (log/info "Create terrain")
  (reset! loki-config (loki/default-config @loki-detail))
  (reset! world (loki/world @loki-config))
  (reset! contours (conrec/contour @world @contour-steps))
  (log/info "Done"))

(defn increase-detail
  "Increase the detail settings for loki."
  []
  (swap! loki-detail inc)
  (log/info "Loki details set to" @loki-detail))

(defn decrease-detail
  "Decrease the detail settings for loki."
  []
  (when (> @loki-detail 1)
    (do
      (swap! loki-detail dec)
      (log/info "Loki details set to" @loki-detail))))

;;====================================================================================================
;; Drawing Functionality

(defn draw-line
  "Draw one line in the given color."
  [line width height line-color]
  (apply quil/stroke line-color)
  (apply quil/fill line-color)
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
  (apply quil/background (:background colors))
  (quil/stroke-weight 2)
  (let [scale (/ width (:width @world))]
    (dorun
      (map #(draw-height-lines (:lines %) scale width height (:line colors)) @contours))))

;;====================================================================================================
;; Event Handling

(defn mouse-pressed
  "Triggered whenever a mouse button is pressed."
  [])

(defn mouse-released
  "Triggered whenever a mouse button is released."
  []
  (let [mx (quil/mouse-x)
        my (quil/mouse-y)
        pm (point/point mx my)]
    ))

(defn key-pressed
  "Trigger actions on key presses."
  []
  (case (quil/key-code)
    171 (increase-detail) ;; +
    173 (decrease-detail) ;; -
    84  (recreate-world!) ;; s
    (log/info "Pressed unhandled key with code" (quil/key-code))))

;;====================================================================================================
;; App Setup

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (apply quil/fill (:background colors))
  (quil/frame-rate 10))

(defn init
  "Initialize Quil sketch."
  [canvas-id]
  (quil/defsketch terrain-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :key-pressed key-pressed))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Terrain Visualization"]
   [:h3 "Usage"]
   [:span
    "Press"
    [:br]
    "t to create a new terrain"
    [:br]
    "+ to increase terrain detail level"
    [:br]
    "- to decrease terrain detail level"]])

(defn ui
  "Draw the basic ui for this visualization."
  [props]
  [:div {:class "row"}
   [:div {:id "hostelement" :class "column left" :on-load init}]
   [:div {:class "column right"}
    [usage]]])

(defn terrain
  "Render html and canvas for terrain visualization."
  [props]
  (reagent/create-class
    {:reagent-render ui
     :component-did-mount #(init "hostelement")}))


