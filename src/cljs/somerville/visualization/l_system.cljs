(ns somerville.visualization.l-system
  (:require
    [somerville.grammar.l-system :as ls]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as line]
    [somerville.color.color :as color]
    [quil.core :as quil :include-macros true]
    [taoensso.timbre :as log]
    [reagent.core :as reagent]))

(def width 1200)
(def height 800)

(def colors
  {:background  (color/rgba  10  10  10)
   :line-low    (color/rgba 241 196  15)
   :line-high   (color/rgba 217  60 110)})


;;====================================================================================================
;; Data Handling

(def koch-rule1 (ls/rule :F '(:F :+ :F :- :F :- :F :+ :F)))
(def koch-rules (list koch-rule1))
(def koch (ls/lsystem :F koch-rules))

(def current (atom koch))
(def rendering (atom (list)))
(def length (reagent/atom 20))

(defn update-angle
  [render-state sym]
  (cond
    (= :+ sym) (+ (:angle render-state) (/ Math/PI 4))
    (= :- sym) (- (:angle render-state) (/ Math/PI 4))
    :else (:angle render-state)))

(defn update-points
  [render-state sym]
  (if (= :F sym)
    (conj (:points render-state) (p/point-at (first (:points render-state)) (:angle render-state) (:length render-state)))
    (:points render-state)))

(defn shorten!
  "Shorten the rendering length"
  []
  (when (> @length 1)
    (do
      (swap! length dec)
      (log/info "Setting length to" @length))))

(defn lengthen!
  "Lengthen the rendering length"
  []
  (swap! length inc)
  (log/info "Setting length to" @length))

(defn produce!
  "Produce the current l-system"
  []
  (reset! current (ls/produce @current)))

;;====================================================================================================
;; Drawing Functionality

(defn render
  []
  (reset! rendering (ls/render @current (p/point 10 10) @length (ls/renderer update-points update-angle))))

(defn draw-koch
  []
  (dorun
    (for [l @rendering]
      (let []
        (quil/stroke 255 255 0)
        (quil/fill 255 255 0)
        (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l)))))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background 0)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  (dorun (draw-koch)))


;;====================================================================================================
;; Event Handling

(defn key-pressed []
  (case (quil/key-code)
    171 (lengthen!) ;; +
    173 (shorten!) ;; -
    80 (produce!) ;; p
    68 (render) ;; d
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
  (quil/defsketch lsystem-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :key-pressed key-pressed))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "L-System Visualization"]
   [:h3 "Usage"]
   [:span
    "Press"
    [:ul
     [:li "+ to progress l-system"]
     [:li "- to regress l-system"]
     [:li "v to draw l-system"]]]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span
    [:ul
     [:li (str "Rendering Length:"  @length)]]]])

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


