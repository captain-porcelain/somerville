(ns somerville.visualization.commons
  (:require
    [somerville.visualization.terrain :as terrain]
    [somerville.visualization.l-system :as l-system]
    [somerville.visualization.gaia :as gaia]
    [somerville.visualization.delaunay :as delaunay]
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
    [:li {:on-click #(reset! current-view "gaia")} "Gaia World Generation"]
    [:li {:on-click #(reset! current-view "l-system")} "L-System Koch Curve"]
    [:li {:on-click #(reset! current-view "terrain")} "Terrain Heightlines"]
    [:li {:on-click #(reset! current-view "delaunay")} "Voronoi by Delaunay"]]])

(defn switch
  "Switch the shown view."
  [props]
  (case @current-view
    "terrain" [terrain/visualize]
    "l-system" [l-system/visualize]
    "gaia" [gaia/visualize]
    "delaunay" [delaunay/visualize]
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

