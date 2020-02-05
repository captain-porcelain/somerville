(ns somerville.visualization.voronoi
  (:require
    [somerville.geometry.line :as line]
    [somerville.geometry.commons :as c]
    [somerville.geometry.point :as p]
    [somerville.geometry.voronoi :as voronoi]
    [somerville.color.color :as color]
    [taoensso.timbre :as log]
    [quil.core :as quil :include-macros true]
    [reagent.core :as reagent]))

(def width 1200)
(def height 800)

(def colors
  {:background     (color/rgba  10  10  10)
   :point-voronoi  (color/rgba   0 204 102)
   :point-delaunay (color/rgba 247  92   3)
   :line-voronoi   (color/rgba 241 196  15)
   :line-delaunay  (color/rgba 217  60 110)})


;;====================================================================================================
;; Data Handling

(def p1 (p/point 200 250))
(def p2 (p/point 500 450))
(def p3 (p/point 100 500))
(def p4 (p/point 250 50))
(def points (reagent/atom (list p1 p2 p3 p4)))
(def sites (atom (voronoi/voronoi @points 0 0 width height)))
(def highlighted (atom nil))

(defn add-point!
  "Add another point to the diagram."
  [point]
  (reset! points (cons point @points))
  (reset! sites (voronoi/voronoi @points 0 0 width height)))

(defn clear!
  "Clear the diagram."
  []
  (reset! points (list))
  (reset! sites (voronoi/voronoi @points 0 0 width height)))

(defn debug!
  "Print debugging informantion"
  []
  (log/info (c/out @sites)))


;;====================================================================================================
;; Drawing Functionality

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

(defn draw
  "This function is called by quil repeatedly."
  []
  (quil/background 0)
  (quil/stroke 0 255 0)
  (quil/fill 0 255 0)
  (dorun
    (for [site (:points @sites)]
      (draw-site site)))
  (dorun
    (for [site (:cells @sites)]
      (draw-cell site))))


;;====================================================================================================
;; Event Handling

(defn mouse-released
  "Handle releasing mouse buttons."
  []
  (add-point! (p/point (quil/mouse-x) (quil/mouse-y))))

(defn key-pressed []
  "Trigger actions on key presses."
  (case (quil/key-as-keyword)
    :c (clear!)
    :d (debug!)
    (log/info (str "pressed key " (quil/key-as-keyword)))))


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
  (quil/defsketch voronoi-sketch
    :host "hostelement"
    :setup setup
    :draw draw
    :size [width height]
    :mouse-released mouse-released
    :key-pressed key-pressed))

(defn usage
  "Show information about usage."
  [props]
  [:div
   [:h2 "Voronoi by intersecting"]
   [:h3 "Usage"]
   [:span
    "Press"
    [:ul
     [:li "left mouse button to add point"]
     [:li "c to clear diagram"]
     [:li "d to print debugging info"]]]])

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


