(ns somerville.visualization.commons
  (:require
    [somerville.visualization.terrain :as terrain]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def current-view (reagent/atom "index"))

(defn index
  "Show visualizations index."
  [props]
  [:div
   [:h2 "Visualization Index"]
   [:ul
    [:li {:on-click #(reset! current-view "terrain")} "Terrain Heightlines"]]])

(defn switch
  "Switch the shown view."
  [props]
  (case @current-view
    "terrain" [terrain/terrain]
    [index]))

(defn framing
  "Show a framing around the visualizations."
  [props]
  (log/info @current-view)
  [:div
   [:h1 {:on-click #(reset! current-view "index")} "Somerville"]
   [switch]])

(defn show
  "Start reagent app."
  []
  (let [app-elem (js/document.getElementById "app")]
    (when-not (nil? app-elem)
      (log/info "Starting visualizations...")
      (reagent/render framing app-elem))))

