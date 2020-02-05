(ns somerville.visualization.tracer
  (:require
    [somerville.fills.line-fill :as lf]
    [somerville.geometry.point :as p]
    [somerville.geometry.commons :as c]
    [somerville.color.color :as color]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [cljs.core.async :refer [chan <! >!]]
    [reagent.core :as reagent])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))

(def width 320)
(def height 320)

(def colors
  {:background     (color/rgba  10  10  10)
   :point-voronoi  (color/rgba   0 204 102)
   :point-delaunay (color/rgba 247  92   3)
   :line-voronoi   (color/rgba 241 196  15)
   :line-delaunay  (color/rgba 217  60 110)})


;;====================================================================================================
;; Data Handling

(def test-image (atom nil))
(def image (atom nil))

(def partitions-all (reagent/atom nil))
(def partitions (reagent/atom nil))

(def threshold-cie (reagent/atom 10))
(def threshold-cluster (reagent/atom 100))

(def alt (atom false))
(def draw-image (reagent/atom true))
(def debug (reagent/atom nil))

(defn decider-fn
  "Check if the colors of two pixels are to be considered the same."
  [p1 p2]
  (let [vfn (fn [p] (apply color/rgba (quil/get-pixel @image (:x p) (:y p))))
        cie (color/cie76 (vfn p1) (vfn p2))]
    (< cie @threshold-cie)))

(defn do-filter
  "Filter partitions that are too small."
  []
  (let [sizes (map lf/cluster-size @partitions-all)
        avg1 (float (/ (reduce + sizes) (count sizes)))
        parts2 (filter #(< @threshold-cluster (lf/cluster-size %)) @partitions-all)
        tmp (log/info (str "filtered to " (count parts2)))]
    (reset! partitions parts2)))

(defn do-partition
  "Find partitions of pixels with a given sameness."
  []
  (let [tmp (log/info "partitionning ...")
        parts1 (lf/clusters (p/point 0 0) 319 319 decider-fn)
        sizes (map lf/cluster-size parts1)
        avg1 (float (/ (reduce + sizes) (count sizes)))
        tmp (log/info (str "... done. found " (count parts1) " partitions with avg " avg1))]
    (reset! partitions-all parts1)))

(defn dopartition
  "Find partions and filter them."
  []
  (go
    (do-partition)
    (do-filter)))

(defn debug-point!
  "Print color of a point."
  [point]
  (reset!
    debug
    (str "Color at " (c/out point) ": "
         (c/out (apply color/rgba (quil/get-pixel @image (:x point) (:y point)))))))

(defn increase-cluster-threshold!
  "Increase the size a cluster must have to not be filtered out."
  []
  (reset! threshold-cluster (+ @threshold-cluster 100))
  (do-filter))

(defn decrease-cluster-threshold!
  "Decrease the size a cluster must have to not be filtered out."
  []
  (when (< 100 @threshold-cluster) (reset! threshold-cluster (- @threshold-cluster 100)))
  (do-filter))

(defn increase-cie-threshold!
  "Increase the threshold by which cie colors are considered the same."
  []
  (reset! threshold-cie (+ @threshold-cie 1))
  (do-filter))

(defn decrease-cie-threshold!
  "Decrease the threshold by which cie colors are considered the same."
  []
  (when (< 100 @threshold-cie) (reset! threshold-cie (- @threshold-cie 1)))
  (do-filter))

(defn toggle-drawing-image!
  "Toggle between drawing the image and the partitions."
  []
  (reset! draw-image (not @draw-image)))


;;====================================================================================================
;; Drawing Functionality

(defn draw-cluster
  "Draw found cluster."
  [cluster]
  (dorun
    (for [l cluster]
      (let [p (:p1 (first cluster))
            dc (apply color/rgba (quil/get-pixel @image (:x p) (:y p)))
            dc (if (lf/in-cluster? (p/point (quil/mouse-x) (quil/mouse-y)) cluster) (color/rgba 255 255 255) dc)]
        (quil/stroke (:r dc) (:g dc) (:b dc))
        (quil/fill (:r dc) (:g dc) (:b dc))
        (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l)))))))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background 0)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  (when @draw-image (quil/image @test-image 0 0))
  (when (not @draw-image) (dorun (for [cl @partitions] (draw-cluster cl)))))


;;====================================================================================================
;; Event Handling

(defn mouse-pressed
  "Handle pressing mouse buttons."
  []
  (debug-point! (p/point (quil/mouse-x) (quil/mouse-y))))

(defn key-released []
  (case (quil/key-code)
     18 (reset! alt false)
     nil))

(defn key-pressed []
  (case (quil/key-code)
     18 (reset! alt true) ;; alt
    171 (if @alt (increase-cluster-threshold!) (increase-cie-threshold!)) ;; +
    173 (if @alt (decrease-cluster-threshold!) (decrease-cie-threshold!)) ;; +
     68 (toggle-drawing-image!) ;; d
     80 (dopartition) ;; p
    (log/info "Pressed unhandled key with code" (quil/key-code))))


;;====================================================================================================
;; App Setup

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (let [bg (:background colors)]
    (quil/fill (:r bg) (:g bg) (:b bg) (:a bg)))
  (quil/frame-rate 1)
  (reset! test-image (quil/load-image  "/img/test-image.jpg"))
  (reset! image (quil/load-image "/img/test-image.jpg")))

(defn init
  "Initialize Quil sketch."
  [canvas-id]
  (quil/defsketch tracer-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :mouse-pressed mouse-pressed
    :key-released key-released
    :key-pressed key-pressed))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Tracer"]
   [:h3 "Usage"]
   [:span
    "Press"
    [:ul
     [:li "p to partition"]
     [:li "d to toggle drawing image"]
     [:li "alt + to increase cluster threshold"]
     [:li "alt - to decrease cluster threshold"]
     [:li "+ to increase cie threshold"]
     [:li "- to decrease cie threshold"]
     [:li "left mouse click to find color at cursor"]]]])

(defn settings
  "Show information current settings."
  [props]
  [:div
   [:h3 "Settings"]
   [:span
    [:ul
     [:li (str "Draw image: " @draw-image)]
     [:li (str "Threshold CIE: " @threshold-cie)]
     [:li (str "Threshold Cluster: " @threshold-cluster)]
     [:li (str "Count partitions: " (count @partitions))]]]
   (when (not (nil? @debug))
     [:div
      [:h3 "Debug"]
      [:div @debug]])])

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



