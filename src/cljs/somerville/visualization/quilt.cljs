(ns somerville.visualization.quilt
  (:require
    [somerville.fills.line-fill :as lf]
    [somerville.geometry.point :as p]
    [somerville.geometry.line :as line]
    [somerville.geometry.voronoi :as voronoi]
    [somerville.color.color :as color]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def test-image (atom nil))
(def partitions-all (atom nil))
(def partitions (atom nil))
(def draw-mode (atom 0))
(def threshold-cie (atom 10))
(def threshold-cluster (atom 0))
(def filename  "../test-image2.jpg")
(def image (atom nil))
(def width 679)
(def height 452)
(def alt (atom false))
(def points (atom (list)))
(def sites (atom nil))

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

(defn draw-intersection
  [i]
  (quil/stroke 0 0 255)
  (quil/fill 0 0 255)
  (quil/rect (:x (:intersection i)) (:y (:intersection i)) 4 4))

(defn draw-bisector
  [bisector]
  (let [y0 (line/solve-line-at (:line bisector) 0)
        y1 (line/solve-line-at (:line bisector) width)]
    (quil/stroke 0 255 0)
    (quil/fill 0 255 0)
    (quil/line 0 y0 width y1)))

(defn draw-site
  [site]
  (quil/stroke 255 0 0)
  (quil/fill 255 0 0)
  (quil/rect (- (:x (:point site)) 2) (- (:y (:point site)) 2) 4 4)
  ;(dorun
  ;  (for [b (:bisectors site)]
  ;    (draw-bisector b)))
  (dorun
      (for [i (:intersections site)]
        (draw-intersection i))))

(def highlighted (atom nil))

(defn draw-cell
  [cell]
  (dorun
    (when (> 5 (p/distance (:point cell) (p/point (quil/mouse-x) (quil/mouse-y))))
      (reset! highlighted cell)))
  (dorun
    (for [l (:lines cell)]
      (let []
        (quil/stroke 255 255 0)
        (quil/fill 255 255 0)
        (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l))))))
  (dorun
    (when (not (nil? @highlighted))
     (for [l (:lines @highlighted)]
        (let []
          (quil/stroke 255 255 255)
          (quil/fill 255 255 255)
          (quil/rect (- (:x (:point @highlighted)) 2) (- (:y (:point @highlighted)) 2) 4 4)
          (quil/line (:x (:p1 l)) (:y (:p1 l)) (:x (:p2 l)) (:y (:p2 l))))))))

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
        starttime (.now js/Date)
        parts1 (lf/clusters (p/point 0 0) (- width 1) (- height 1) decider-fn)
        endtime (.now js/Date)
        sizes (map lf/cluster-size parts1)
        avg1 (float (/ (reduce + sizes) (count sizes)))
        tmp (dorun (println (str "... done. found " (count parts1) " partitions with avg of " avg1 " points in " (- endtime starttime) " ms.")))]
    (reset! partitions-all parts1)))

(defn dopartition
  []
  (do-partition)
  (do-filter))

(defn dovoronoi
  []
  (reset! points (map lf/cluster-center @partitions-all))
  (reset! sites (voronoi/voronoi @points 0 0 width height)))

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background 0)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  (when (= 0 @draw-mode) (quil/image @test-image 0 0))
  (when (= 1 @draw-mode) (dorun (for [cl @partitions] (draw-cluster cl))))
  (when (= 2 @draw-mode) (dorun (for [site (:points @sites)] (draw-site site))))
  (when (= 2 @draw-mode) (dorun (for [site (:cells @sites)] (draw-cell site)))))

(defn mouse-pressed [])
(defn mouse-released [])

(defn key-released []
  (if (= (quil/key-code) 18) ; alt
    (reset! alt false)))

(defn key-pressed []
  ;;(dorun (println (str "pressed code " (quil/key-code))))
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
  (if (= (quil/key-code) 86) ; v
    (dovoronoi))
  (if (= (quil/key-code) 68) ; d
    (let [dm (+ 1 @draw-mode)
          dm (if (= 3 dm) 0 dm)]
      (reset! draw-mode dm))))


;;====================================================================================================
;; App Setup

(defn setup
  "This function is called by quil once before drawing"
  []
  (quil/smooth)
  (let [bg (:background colors)]
    (quil/fill (:r bg) (:g bg) (:b bg) (:a bg)))
  (quil/frame-rate 1)
  (reset! test-image (quil/load-image filename))
  (reset! image (quil/load-image filename)))

(defn init
  "Initialize Quil sketch."
  [canvas-id]
  (quil/defsketch quilt-sketch
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
   [:h2 "Voronoi Quilt Generator"]
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
     [:li (str "Count points: " (count @points))]]]])

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



