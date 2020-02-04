(ns somerville.visualization.commons
  (:require
    [somerville.visualization.terrain :as terrain]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(defn framing
  "Show a framing around the visualizations."
  [props]
  [:div
   [terrain/terrain]])

(defn show
  "Start reagent app."
  []
  (let [app-elem (js/document.getElementById "app")]
    (when-not (nil? app-elem)
      (log/info "Starting visualizations...")
      (reagent/render framing app-elem))))

