(ns somerville.visualization.tracer
  (:require
    [somerville.fills.line-fill :as lf]
    [somerville.geometry.point :as p]
    [somerville.color.color :as color]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def width 320)
(def height 320)
(def test-image (atom nil))
(def partitions-all (atom nil))
(def partitions (reagent/atom nil))
(def draw-fill (atom true))
(def threshold-cie (atom 10))
(def threshold-cluster (atom 100))
(def image (atom nil))
(def alt (atom false))

(def colors
  {:background     (color/rgba  10  10  10)
   :point-voronoi  (color/rgba   0 204 102)
   :point-delaunay (color/rgba 247  92   3)
   :line-voronoi   (color/rgba 241 196  15)
   :line-delaunay  (color/rgba 217  60 110)})

(defn decider-fn
  [p1 p2]
  (let [vfn (fn [p] (color/rgba (quil/get-pixel @image (:x p) (:y p))))
        cie (color/cie76 (vfn p1) (vfn p2))]
    (< cie @threshold-cie)))

(defn draw-cluster
  [cluster]
  (dorun
    (for [l cluster]
      (let [p (:p1 (first cluster))
            dc (color/rgba (quil/get-pixel @image (:x p) (:y p)))
            dc (if (lf/in-cluster? (p/point (quil/mouse-x) (quil/mouse-y)) cluster) (color/rgba 255 255 255) dc)]
        (quil/stroke (:r dc) (:g dc) (:b dc))
        (quil/fill (:r dc) (:g dc) (:b dc))
        (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l)))))))

(defn do-filter
  []
  (let [sizes (map lf/cluster-size @partitions-all)
        avg1 (float (/ (reduce + sizes) (count sizes)))
        parts2 (filter #(< @threshold-cluster (lf/cluster-size %)) @partitions-all)
        tmp (dorun (println (str "filtered to " (count parts2))))]
    (reset! partitions parts2)))

(defn do-partition
  []
  (let [tmp (dorun (println "partitionning ..."))
        parts1 (lf/clusters (p/point 0 0) 319 319 decider-fn)
        sizes (map lf/cluster-size parts1)
        avg1 (float (/ (reduce + sizes) (count sizes)))
        tmp (dorun (println (str "... done. found " (count parts1) " partitions with avg " avg1)))]
    (reset! partitions-all parts1)))

(defn dopartition
  []
  (do-partition)
  (do-filter))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background 0)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  (when (not @draw-fill) (quil/image @test-image 0 0))
  ;(when @draw-fill (dorun (for [cl @partitions] (draw-cluster cl)))))
  )

(defn mouse-pressed [])
(defn mouse-released [])

(defn key-released []
  (if (= (quil/key-code) 18) ; alt
    (reset! alt false)))

(defn key-pressed []
  ;(dorun (println (str "pressed code " (quil/key-code))))
  (if (= (quil/key-code) 18) ; alt
    (reset! alt true))
  (if (= (quil/key-code) 521) ; +
    (if @alt
      (let []
        (reset! threshold-cluster (+ @threshold-cluster 100))
        (dorun (println (str "threshold for cluster size is " @threshold-cluster)))
        (do-filter))
      (let []
        (reset! threshold-cie (+ @threshold-cie 1))
        (dorun (println (str "threshold for color difference is " @threshold-cie))))))
  (if (= (quil/key-code) 45) ; -
    (if @alt
      (let []
        (reset! threshold-cluster (- @threshold-cluster 100))
        (dorun (println (str "threshold for cluster size is " @threshold-cluster)))
        (do-filter))
      (let []
        (reset! threshold-cie (- @threshold-cie 1))
        (dorun (println (str "threshold for color difference is " @threshold-cie))))))
  (if (= (quil/key-code) 80) ; p
    (dopartition))
  (if (= (quil/key-code) 68) ; d
    (reset! draw-fill (not @draw-fill))))


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
    :mouse-released mouse-released
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
     [:li (str "Count partitions: " (count @partitions))]]]])

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



